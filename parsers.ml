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

exception X_this_should_not_happen;;


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
    
(* let make_commented nt = make_paired (star nt_comment_line) (star nt_comment_line) nt;; *)

(* let whitespace_or_comment = disj (pack nt_whitespace (fun e -> Nil)) nt_comment_line ;;
let make_spaced_or_commented nt = make_paired (star whitespace_or_comment) (star whitespace_or_comment) nt;; *)

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
nt;;

(*3.3.3 Symbol*)
let digit = range '0' '9';;

let nt_symbol =
    let lowercase_letters = range 'a' 'z' in
    let uppercase_letters = range 'A' 'Z' in
    let punctuation = disj_list [char ':';char '!'; char '$'; char '^'; char '*'; char '-'; char '_'; char '='; char '+'; char '<'; char '>'; char '/'; char '?'] in    
    let norm_uppercase = pack uppercase_letters lowercase_ascii in
    let nt = disj_list [lowercase_letters; norm_uppercase; punctuation; digit] in
    let nt = plus nt in
    let nt = pack nt (fun e -> let str = list_to_string e in Symbol(str)) in
    nt;;

(*3.3.2 Number*)
let not_digit = const (fun ch -> ch < '0' || ch > '9');;

(*int*)
let nt_int = 
    let nt_body = pack (not_followed_by (plus digit) (char '.')) (function e -> int_of_string ((list_to_string e))) in
    let nt_plus_op = char '+' in
    let nt_minus_op = char '-' in
    let nt_op = disj nt_minus_op nt_plus_op in
    let nt_signed = pack (caten nt_op nt_body) 
    (function (op,num) -> if (op = '-') then (-1)*(num) else num) in
    let nt = disj nt_signed nt_body in
    nt;;


let nt_int_packed = 
    let nt = not_followed_by nt_int nt_symbol in
    pack nt (fun e -> Int(e));;
 
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
    let nt = disj nt_signed nt_body in
    nt;;
    (*number*)

let nt_float_packed = 
    let nt_symbols_not_e = diff nt_symbol (word_ci "e") in
    let nt = not_followed_by nt_float nt_symbols_not_e in
    pack nt (fun e -> Float(e));;
    
    (*number*)

(*4.1 Scientific notation*)
let nt_scientific_notation = 
    let nt_int_to_float = pack nt_int (fun e -> float_of_int e) in
    let nt = disj nt_int_to_float nt_float in
    let nt_e = word_ci "e" in
    let nt = caten nt (caten nt_e nt_int_to_float) in
    (* let nt = not_followed_by nt nt_symbol in *)
    let nt = pack nt (fun (num, (e, exp)) -> let n = num *. (10. ** exp) in Float(n)) in
    nt;;

let nt_number = 
    let nt = disj_list [nt_scientific_notation;nt_float_packed; nt_int_packed] in
    let nt = pack nt (function e -> Number(e)) in
    nt;;

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
    nt;;

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
    nt;;

(*3.3.6 Nil*)
let nt_nil = 
    let prefix = char '(' in
    let postfix = char ')' in
    let body = disj nt_comment_line (pack nt_whitespace (fun e -> Nil)) in
    let nt = caten prefix (caten (star body) postfix) in
    let nt = pack nt (fun e -> Nil) in
    nt;;

(*3.3.7 Pair*)
(* let nt_proper_list =
    let prefix = char '(' in
    let postfix = char ')' in
    let rec nt_proper_list_rec () =  *)



(* sexp *)

let rec nt_sexpr str = 
    let sexpr_disj = disj_list [
            nt_boolean;
            nt_char;
            nt_number;
            nt_string;
            nt_symbol;
            nt_list;
            nt_dotted_list;
            nt_quote;
            nt_quasi_quote;
            nt_unquote;
            nt_unquote_and_splice
            (* nt_tag_expr; *)
            (* nt_comment_sexpr *)
           ] in
    (make_spaced_or_commented sexpr_disj) str
    and nt_list s = 
        let prefix = char '(' in
        let postfix = char ')' in
        let body = star nt_sexpr in
        let nt = caten prefix (caten body postfix) in
        pack nt (
            function (_,(e,_)) -> match e with
            |[] -> Nil
            |lst -> List.fold_right (fun sexpr1 sexpr2 -> Pair(sexpr1, sexpr2)) lst Nil
        ) s
    and nt_dotted_list s = 
        let prefix = char '(' in
        let postfix = char ')' in
        let nt_dot = char '.' in
        let body = caten (plus nt_sexpr) (caten nt_dot nt_sexpr) in
        let nt = caten prefix (caten body postfix) in
        pack nt (
            function (_,(e,_)) -> match e with
            |(a, (_, b)) -> List.fold_right (fun sexpr1 sexpr2 -> Pair(sexpr1, sexpr2)) a b
        ) s
    and nt_quote s = 
        let prefix = word "'" in
        let nt = caten prefix nt_sexpr in
        pack nt (function (_, e) -> Pair(Symbol("quote"), Pair(e, Nil)))
        s
    and nt_quasi_quote s = 
        let prefix = word "`" in
        let nt = caten prefix nt_sexpr in
        pack nt (function (_, e) -> Pair(Symbol("quasiquote"), Pair(e, Nil)))
        s
    and nt_unquote s = 
        let prefix = word "," in
        let nt = caten prefix nt_sexpr in
        pack nt (function (_, e) -> Pair(Symbol("unquote"), Pair(e, Nil)))
        s
    and nt_unquote_and_splice s = 
        let prefix = word ",@" in
        let nt = caten prefix nt_sexpr in
        pack nt (function (_, e) -> Pair(Symbol("unquote-splicing"), Pair(e, Nil)))
        s
    and nt_comment_sexpr s =
        let prefix = word "#;" in
        let body = nt_sexpr in
        let nt = caten prefix body in
        (pack nt (fun e -> Nil)) 
        s
    and make_spaced_or_commented s = 
        (* let nt_not_last_comment_sexpr = not_followed_by (pack nt_comment_sexpr (fun e -> Nil)) (pack (nt_end_of_input) (fun e -> Nil)) in *)
        let whitespace_or_comment = disj_list [(pack nt_whitespace (fun e -> Nil));nt_comment_line;nt_comment_sexpr] in
        let nt1 nt = make_paired (star whitespace_or_comment) (star whitespace_or_comment) nt in
        nt1 s;;

    (* and nt_tag_expr s =
        let prefix = word "#{" in
        let postfix = word "}" in
        let eq_sign = word "=" in
        let symbol_name = pack nt_symbol (
            function e -> match e with
            | Symbol(name) -> name) in
        let nt_ref = caten prefix (caten (symbol_name) postfix) in
        let nt_tag_prefix = caten nt_ref eq_sign in
        let nt_tag = caten nt_tag_prefix nt_sexpr in
        let nt = pack nt_tag 
        (function  (((_, (name, _)),_), (sexp,_)) -> 
            let rec throw_err_if_tagged_twice sexp_rec = 
                let is_same_tag = sexpr_eq sexp_rec Tag(name, sexp) in
                if is_same_tag then raise X_this_should_not_happen else match sexp_rec with 
                    | (a,b) -> if (sexpr_eq a Tag(name, sexp)) then raise X_this_should_not_happen else
                        throw_err_if_tagged_twice b
                    | e -> None
                   
            in        
            throw_err_if_tagged_twice sexp) in
        nt;;       *)


    
        end;; (* end of struct PC *)