#use "pc.ml";;
#use "parsers.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Int of int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | TaggedSexpr of string * sexpr
  | TagRef of string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Int n1), Number(Int n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | TaggedSexpr(name1, expr1), TaggedSexpr(name2, expr2) -> (name1 = name2) && (sexpr_eq expr1 expr2) 
  | TagRef(name1), TagRef(name2) -> name1 = name2
  | _ -> false;;
  
module Reader
(* : sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
end *)
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;
  



(* let parse_sexpr string_list =
(*1. apply disj_list on list of nts (according to CFG) and string_list *)
  OP.disj_list (nt1, nt2, nt3...) string_list *)

let read_sexpr string =
  let (sexpr, s) = (parse_sexpr (string_to_list string)) in
  if (s = [])
  then sexpr
  else raise X_no_match;;


let read_sexprs string =
    let (sexpr_list, s) = ((PC.star parse_sexpr) (string_to_list string)) in
    sexpr_list;;

end;; (* struct Reader *)


module NT = struct

(* the parsing combinators defined here *)
  
exception X_not_yet_implemented;;

exception X_no_match;;

(* parser for identifying whitespaces.
 The definition is simple: Essentially anything less than or equal to the space character is a whitespace: *)
let nt_whitespace = const (fun ch -> ch <= ' ');;

let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

let make_spaced nt = make_paired (star nt_whitespace) (star nt_whitespace) nt;;

(* parser that reads hex number and returns decimal convertion *)
let nt_hex =
  let make_NT_digit ch_from ch_to displacement =
    let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
    let nt = pack nt (let delta = (Char.code ch_from) - displacement in
		      fun ch -> (Char.code ch) - delta) in
    nt in
  let nt = disj (make_NT_digit '0' '9' 0)
		(make_NT_digit 'a' 'f' 10) in
  let nt = disj nt (make_NT_digit 'A' 'F' 10) in
  let nt = plus nt in
  let nt = pack nt (fun digits ->
		    List.fold_left (fun a b -> 16 * a + b) 0 digits) in
  let nt1 = caten (word_ci "0x") nt in
  let nt1 = pack nt1 (function (_0x, e) -> e) in
  let nt2 = caten nt (word_ci "h")  in
  let nt2 = pack nt2 (function (h_, e) -> e) in
  let nt = disj nt1 nt2 in
  make_spaced nt;;
end;; (* end of struct PC *)


(* testing *)
open NT;;

test_string (make_spaced (word("moshe"))) "   moshe   ";;
test_string nt_hex "    0x35";;