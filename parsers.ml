#use "pc.ml";;
open PC;;

type expr =
  | Number of int
  | Plus of expr * expr
  | Mult of expr * expr
  | Power of expr * expr;;

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

module NT = struct

(* the parsing combinators defined here *)
  
exception X_not_yet_implemented;;

exception X_no_match;;

(* 3.2.1 parser for identifying whitespaces *)
let nt_whitespace = const (fun ch -> ch <= ' ');;
(* 3.2.1 parser for identifying line comments *)
let nt_semicolon = const (fun ch -> ch = ';');;
let nt_eol = const (fun ch -> ch = '\n');;
let nt_eol_eoi = disj nt_semicolon nt_end_of_input
let make_line_comment nt = make_paired nt_semicolon (star nt_whitespace) nt;;
(* abstract parser that skips nt_right and nt_left results from left and right of nt *)
let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

(* parser that skips whitesapes from left and right of nt *)
let make_spaced nt = make_paired (star nt_whitespace) (star nt_whitespace) nt;;

(* parser that reads hex number and returns decimal rep *)
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
  let nt = caten (word_ci "0x") nt in
  let nt = pack nt (function (_0x, e) -> e) in  
  (* let nt1 = caten (word_ci "0x") nt in
  let nt1 = pack nt1 (function (_0x, e) -> e) in
  let nt2 = caten nt (word_ci "h")  in
  let nt2 = pack nt2 (function (h_, e) -> e) in
  let nt = disj nt1 nt2 in *)
  make_spaced nt;;

  (* parser that reads parenthesi *)
let make_nt_parenthesized_expr nt =
  let nt1 = make_paired (make_spaced (char '(')) 
			(make_spaced (char ')')) nt in
  let nt2 = make_paired (make_spaced (char '[')) 
			(make_spaced (char ']')) nt in
  let nt3 = make_paired (make_spaced (char '{'))
			(make_spaced (char '}')) nt in
  let nt = disj nt1 (disj nt2 nt3) in
  nt;;

(*	 <expr> ::= <L0>
	   <L0> ::= <L1> {`+' <L1> }*
	   <L1> ::= <L2> {`*' <L2> }*
	   <L2> ::= <L3> {`^' <L3> }*
	   <L3> ::= <Number> | <Parenthesized>
<Parenthesized> ::= `(' <L0> `)' | `[' <L0> `]' | `{' <L0> `}' *)

  let nt_expr = 
    let rec nt_parenthesized_expr s =
    (*<Parenthesized> ::= `(' <L0> `)' | `[' <L0> `]' | `{' <L0> `}' *)
        make_nt_parenthesized_expr nt_L0 s
    and nt_L3 s =
        (*<L3> ::= <Number> | <Parenthesized>*)
        disj nt_parenthesized_expr
        (pack nt_hex (fun e -> Number e))
        s 
    and nt_L2 s =
        (*<L2> ::= <L3> {`^' <L3> }* *)
        let nt = caten (make_spaced (char '^'))
            nt_L3 in
        let nt = pack nt (function (_, e) -> e) in
        let nt = star nt in
        let nt = caten nt_L3 nt in
        let nt = pack nt (fun (e1, es) ->
                List.fold_left (fun a b -> Power(a, b))
                        e1
                        es) in
        nt s
    and nt_L1 s =
            (*<L1> ::= <L2> {`*' <L2> }* *)
        let nt = caten (make_spaced (char '*'))
            nt_L2 in
        let nt = pack nt (function (_, e) -> e) in
        let nt = star nt in
        let nt = caten nt_L2 nt in
        let nt = pack nt (function (e1, es) ->
                    List.fold_left (fun a b -> Mult(a, b))
                            e1
                            es) in
        nt s
    and nt_L0 s =
        let nt = caten (make_spaced (char '+'))
            nt_L1 in
        let nt = pack nt (function (_, e) -> e) in
        let nt = star nt in
        let nt = caten nt_L1 nt in
        let nt = pack nt (function (e1, es) ->
                    List.fold_left (fun a b -> Plus(a, b))
                            e1
                            es) in
        nt s in
    nt_L0;;

end;; (* end of struct PC *)


(* testing *)
open NT;;

test_string (make_spaced (word("moshe"))) "   moshe   ";;
test_string nt_hex "    0x35";;
test_string nt_expr "0x234";;
test_string nt_expr "0x2 * 0x3 + 0x4 * 0x5";;