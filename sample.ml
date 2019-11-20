#use "parsers.ml";;

(* testing *)

open NT;;

test_string (make_spaced (word("moshe"))) "   moshe   ";;
test_string nt_hex "    0x35";;
(* test_string nt_boolean "         #t";; *)
(* test_string nt_int "-0";; *)
test_string nt_number "-0998.0";;



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
  (* let nt_expr = 
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
    nt_L0;; *)