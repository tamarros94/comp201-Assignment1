(* pc.ml
 * A parsing-combinators package for ocaml
 *
 * Prorammer: Mayer Goldberg, 2018
 *)

(* general list-processing procedures *)

(* applies boolean function to every element in list and return true if at least one element is true*)
let rec ormap f s =
  match s with
  | [] -> false
  | car :: cdr -> (f car) || (ormap f cdr);;

(* applies boolean function to every element in list and return false if at least one element is false*)
let rec andmap f s =
  match s with
  | [] -> true
  | car :: cdr -> (f car) && (andmap f cdr);;	 

(* turn uppercase char to lowercase *)
let lowercase_ascii  =
  let delta = int_of_char 'A' - int_of_char 'a' in
  fun ch ->
  if ('A' <= ch && ch <= 'Z')
  then char_of_int ((int_of_char ch) - delta)
  else ch;;

(* turns string to list of chars*)
let string_to_list str =
  let rec loop i limit =
    if i = limit then []
    else (String.get str i) :: (loop (i + 1) limit)
  in
  loop 0 (String.length str);;

(* turns list to string*)
let list_to_string s =
  String.concat "" (List.map (fun ch -> String.make 1 ch) s);;

module PC = struct

(* the parsing combinators defined here *)
  
exception X_not_yet_implemented;;

exception X_no_match;;

(* The const PC takes a predicate (char -> bool), and return a
parser that recognizes this character *)
let const pred =
  function 
  | [] -> raise X_no_match
  | e :: s ->
     if (pred e) then (e, s)
     else raise X_no_match;;

(* We try to parse the head of s using nt1
▶ If we succeed, we get e1 and the remaining chars s
▶ We try to parse the head of s (what remained after nt1) using nt2
▶ If we succeed, we get e2 and the remaining chars s
▶ We return the pair of e1 & e2, as well as the remaining chars *)
let caten nt1 nt2 s =
  let (e1, s) = (nt1 s) in
  let (e2, s) = (nt2 s) in
  ((e1, e2), s);;

(* pack takes a non-terminal nt and a function f
▶ returns a parser that recognizes the same language as nt
▶ …but which applies f to whatever was matched *)
let pack nt f s =
  let (e, s) = (nt s) in
  ((f e), s);;

(* parser that recognizes ε-productions
This is like I (evar hayehida) -> caten nt nt_epsilon = caten nt_epsilon nt = nt *)
let nt_epsilon s = ([], s);;

(* given a list of non-terminals:
    returns function that receives list s and:
      for each pair of non terminals in list s: performs caten and concatenates result list until nt_epsilon
     *)
let caten_list nts =
  List.fold_right
    (fun nt1 nt2 ->
     pack (caten nt1 nt2)
	  (fun (e, es) -> (e :: es)))
    nts
    nt_epsilon;;

(* We try to parse the head of s using nt1
▶ If we succeed, then the call to nt1 returns normally
▶ If we fail we try to parse the head of s using nt2 *)
let disj nt1 nt2 =
  fun s ->
  try (nt1 s)
  with X_no_match -> (nt2 s);;

(*parser that always fails
disj nt nt_none ≡ disj nt_none nt ≡ nt*)
let nt_none _ = raise X_no_match;;

(* given a list of non-terminals:
    returns function that receives list s and:
      for each pair of non terminals in list: performs disj on s until nt_none
     *)
let disj_list nts = List.fold_right disj nts nt_none;;

(* kind of like lazy-eval: supposed to handle recursive CFG like A->A|B
To implement recursive parsers, we need to delay the evaluation of the recursive non-terminal
▶ A thunk is a procedure that takes zero arguments
▶ Thunks are used to delay evaluation*)
let delayed thunk s =
  thunk() s;;

(*parser that recognizes the end of the
input stream (and fails otherwise)*)
let nt_end_of_input = function
  | []  -> ([], [])
  | _ -> raise X_no_match;;

(* applies nt on every element on the list s until there's no match*)
let rec star nt s =
  try let (e, s) = (nt s) in
      let (es, s) = (star nt s) in
      (e :: es, s)
  with X_no_match -> ([], s);;

(* given a non-terminal -> 
    returns a function that receives a list:
      (at least one)production result concatenated to all production rules possibilities contcatinated*)
let plus nt =
  pack (caten nt (star nt))
       (fun (e, es) -> (e :: es));;

(* We might want to attach an arbitrary predicate to serve as a guard
for a parser, so that the parser succeeds only if the matched object
satisfies the guard.*)
let guard nt pred s =
  let ((e, _) as result) = (nt s) in
  if (pred e) then result
  else raise X_no_match;;
  
  (* applies nt1 on s and then nt2 on the same s.
  Scenarios:
    1. nt1 raises X_no_match -> diff raises throws X_no_match
    2. nt1 passes + nt2 throws X_no_match -> 1st part is matched with 'Some' and result is returned
    3. nt1 passes + nt2 passes -> 1st part is matched with 'None' and diff raises X_no_match 
  returns a parser that accepts the language of nt1 WITHOUT the language of nt2
  e.g: I want nt_no_digits then I can run diff nt_all nt_digits*)
let diff nt1 nt2 s =
  match (let result = nt1 s in
	 try let _ = nt2 s in
	     None
	 with X_no_match -> Some(result)) with
  | None -> raise X_no_match
  | Some(result) -> result;;

(* applies nt1 on s and then applies nt2 on s' leftover of nt1. 
  e.g: *)
let not_followed_by nt1 nt2 s =
  match (let ((_, s) as result) = (nt1 s) in
	 try let _ = (nt2 s) in
	     None
	 with X_no_match -> (Some(result))) with
  | None -> raise X_no_match
  | Some(result) -> result;;
	  
(*applies nt on s, if there's no match, (None,s) is returned instead of raising error, and (Some(e), s) otherwise*)
let maybe nt s =
  try let (e, s) = (nt s) in
      (Some(e), s)
  with X_no_match -> (None, s);;

(* useful general parsers for working with text *)

let make_char equal ch1 = const (fun ch2 -> equal ch1 ch2);;

let char = make_char (fun ch1 ch2 -> ch1 = ch2);;

let char_ci =
  make_char (fun ch1 ch2 ->
	     (lowercase_ascii ch1) =
	       (lowercase_ascii ch2));;

let make_word char str = 
  List.fold_right
    (fun nt1 nt2 -> pack (caten nt1 nt2) (fun (a, b) -> a :: b))
    (List.map char (string_to_list str))
    nt_epsilon;;

let word = make_word char;;

let word_ci = make_word char_ci;;

let make_one_of char str =
  List.fold_right
    disj
    (List.map char (string_to_list str))
    nt_none;;

let one_of = make_one_of char;;

let one_of_ci = make_one_of char_ci;;

let nt_whitespace = const (fun ch -> ch <= ' ');;

let make_range leq ch1 ch2 (s : char list) =
  const (fun ch -> (leq ch1 ch) && (leq ch ch2)) s;;

let range = make_range (fun ch1 ch2 -> ch1 <= ch2);;

let range_ci =
  make_range (fun ch1 ch2 ->
	      (lowercase_ascii ch1) <=
		(lowercase_ascii ch2));;

let nt_any (s : char list) = const (fun ch -> true) s;;

let trace_pc desc nt s =
  try let ((e, s') as args) = (nt s)
      in
      (Printf.printf ";;; %s matched the head of \"%s\", and the remaining string is \"%s\"\n"
		     desc
		     (list_to_string s)
		     (list_to_string s') ;
       args)
  with X_no_match ->
    (Printf.printf ";;; %s failed on \"%s\"\n"
		   desc
		   (list_to_string s) ;
     raise X_no_match);;

(* testing the parsers *)

let test_string nt str =
  let (e, s) = (nt (string_to_list str)) in
  (e, (Printf.sprintf "->[%s]" (list_to_string s)));;

end;; (* end of struct PC *)

(* end-of-input *)
