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
    let nt_semicolon = char ';' in
    let nt_eol = char (char_of_int 10) in
    let nt_all_but_eol = diff nt_any nt_eol in
    let nt_end_of_comment = disj nt_eol (pack nt_end_of_input (fun (dummy) -> 'd')) in
    let nt = caten nt_semicolon (star nt_all_but_eol) in
    let nt = caten nt nt_end_of_comment in
    let nt = pack nt (fun e -> Nil) in
    make_spaced nt;;
    

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
    (* let nt = pack nt (function (_, false) -> (Bool false)) in  *)
    let nt = pack nt (function (_0x, e) -> (Bool e)) in
(make_spaced nt);;

(*3.3.2 Number*)
let digit = range '0' '9';;
let not_digit = const (fun ch -> ch < '0' || ch > '9');;
(*int*)
let nt_int = 
    let nt_body = pack (not_followed_by (plus digit) (char '.')) (function e -> int_of_string ((list_to_string e))) in
    let nt_plus_op = char '+' in
    let nt_minus_op = char '-' in
    let nt_op = disj nt_minus_op nt_plus_op in
    let nt_signed = pack (caten nt_op nt_body) 
    (function (op,num) -> if (op = '-') then (-1)*(num) else num) in
    let nt = pack (disj nt_signed nt_body) (fun e -> Int(e)) in                                                                                                                                                        
    (make_spaced nt);;    

let nt_dot = char '.' ;;
let nt_form = caten (plus digit) (caten nt_dot (plus digit)) ;;
(*float*)
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
    (make_spaced nt);;
    (*number*)
let nt_number = make_spaced (pack (disj nt_int nt_float) (function e -> Number(e)));;

(*3.3.3 Symbol*)
let nt_symbol =
    let lowercase_letters = range 'a' 'z' in
    let uppercase_letters = range 'A' 'Z' in
    let punctuation = disj_list [char '.';char '!'; char '$'; char '^'; char '*'; char '-'; char '_'; char '='; char '+'; char '<'; char '>'; char '/'; char '?'] in    
    let norm_uppercase = pack uppercase_letters lowercase_ascii in
    let nt = disj_list [lowercase_letters; norm_uppercase; punctuation; digit] in
    let nt = star nt in
    let nt = pack nt (fun e -> let str = list_to_string e in Symbol(str)) in
    make_spaced nt;;

(*3.3.4 String*)
let nt_string = 
    let string_literal_char = diff nt_any (disj (char (char_of_int 34)) (char (char_of_int 92))) in
    let string_meta_char = disj_list [
        pack (word "\\\\") (fun e -> char_of_int 92);
        pack (word "\\\"") (fun e -> char_of_int 34);
        pack (word "\\t") (fun e -> char_of_int 9);
        pack (word "\\f") (fun e -> char_of_int 12);
        pack (word "\\n") (fun e -> char_of_int 10);
        pack (word "\\r") (fun e -> char_of_int 13)
        ] in
    let nt_body = disj string_literal_char string_meta_char in
    let nt_double_quote = char (char_of_int (34)) in
    let nt = caten nt_double_quote (caten (star nt_body) nt_double_quote) in
    let nt = pack nt (fun (_, (e, _)) -> String(list_to_string e)) in
    make_spaced nt;;

(*3.3.5 Char*)
let nt_char =
    let char_prefix = word "#\\" in
    let visible_simple_char = const (fun ch -> (int_of_char ch) > 32) in
    let named_char = disj_list [
        pack (word "nul") (fun e -> char_of_int 0);
        pack (word "newline") (fun e -> char_of_int 10);
        pack (word "return") (fun e -> char_of_int 13);
        pack (word "tab") (fun e -> char_of_int 9);
        pack (word "page") (fun e -> char_of_int 12);
        pack (word "space") (fun e -> char_of_int 32)
        ] in
    let nt = disj visible_simple_char named_char in
    let nt = pack (caten char_prefix nt) (fun (_, e) -> Char(e)) in
    make_spaced nt;;

let nt_nil = 
    let prefix = char '(' in
    let postfix = char ')' in
    let body = disj nt_comment_line (pack nt_whitespace (fun e -> Nil)) in
    let nt = caten prefix (caten (star body) postfix) in
    let nt = pack nt (fun e -> Nil) in
    make_spaced nt;;



end;; (* end of struct PC *)