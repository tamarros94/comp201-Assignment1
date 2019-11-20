#use "pc.ml";;
open PC;;

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

(* type expr =
  | Number of int
  | Plus of expr * expr
  | Mult of expr * expr
  | Power of expr * expr;; *)

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

module NT = struct

(* the parsing combinators defined here *)
  
exception X_not_yet_implemented;;

exception X_no_match;;

(*abstract parsers*)
let nt_all_but c = star (const (fun ch -> ch != c))
(* abstract parser that skips nt_right and nt_left results from left and right of nt *)
let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

(* 3.2.1 Whitespaces *)
let nt_whitespace = const (fun ch -> ch <= ' ');;
(* parser that skips whitesapes from left and right of nt *)
let make_spaced nt = make_paired (star nt_whitespace) (star nt_whitespace) nt;;

(* 3.2.2 Line comments *)
let nt_comment_line = 
    let nt_semicolon = const (fun ch -> ch ==';') in
    let nt_eol = const (fun ch -> ch == '\n') in
    let nt_all_but_eol = nt_all_but '\n' in
    let nt_end_of_comment = disj nt_eol (pack nt_end_of_input (fun (dummy) -> 'a')) in
    let nt = caten nt_semicolon nt_all_but_eol in
    let nt = caten nt nt_end_of_comment in
    (pack nt (fun (dummy) -> Nil));;


    

(* 3.2.3 Sexpr comments *)
(* let nt_comment_sexpr = 
    let start_comment = word_ci "#;" in
    let packed_parse_sexpr = pack nt_sexpr (fun e -> Nil);; *)

(*3.3.1 Boolean*)
let nt_boolean =
    let nt_hashtag = char '#' in
    let nt_f = word_ci "f" in
    let nt_t = word_ci "t" in
    let nt_false = pack nt_f (fun f -> false) in
    let nt_true = pack nt_t (fun t -> true) in
    let nt = disj nt_false nt_true in
    let nt = caten nt_hashtag nt in
    let nt = make_spaced nt in
    (* let nt = pack nt (function (_, false) -> (Bool false)) in  *)
    let nt = pack nt (function (_0x, e) -> (Bool e)) in
nt;;

(*3.3.2 Number*)
let digit = range '0' '9';;
let not_digit = const (fun ch -> ch < '0' || ch > '9');;

let nt_int = 
    let nt_body = pack (not_followed_by (plus digit) (char '.')) (function e -> int_of_string ((list_to_string e))) in
    let nt_plus_op = char '+' in
    let nt_minus_op = char '-' in
    let nt_op = disj nt_minus_op nt_plus_op in
    let nt_signed = pack (caten nt_op nt_body) 
    (function (op,num) -> if (op = '-') then (-1)*(num) else num) in
    let nt = pack (disj nt_signed nt_body) (fun e -> Int(e)) in                                                                                                                                                        
    nt;;    

let nt_dot = char '.' ;;
let nt_form = caten (plus digit) (caten nt_dot (plus digit)) ;;

let nt_float =
    let nt_dot = char '.' in
    let nt_form = caten (plus digit) (caten nt_dot (plus digit)) in
    let nt_body = pack nt_form 
    (function (a,(b, c)) -> float_of_string ((list_to_string a) ^ "." ^ (list_to_string c))) in
    let nt_plus_op = char '+' in
    let nt_minus_op = char '-' in
    let nt_op = disj nt_minus_op nt_plus_op in
    let nt_signed = pack (caten nt_op nt_body) 
    (function (op,num) -> if (op = '-') then (-1.0)*.(num) else num) in
    let nt = pack (disj nt_signed nt_body) (fun e -> Float(e)) in                                                                                                                                                        
    nt;;
let nt_number = pack (disj nt_int nt_float) (function e -> Number(e))

(*3.3.3 Symbol*)




end;; (* end of struct PC *)