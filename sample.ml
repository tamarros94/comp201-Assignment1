#use "parsers.ml";;

(* testing *)

open NT;;

test_string (make_spaced (word("moshe"))) "   moshe   ";;
test_string nt_hex "    0x35";;
test_string nt_boolean "/#f";;


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