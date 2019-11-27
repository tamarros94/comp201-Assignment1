#use "parsers.ml";;

(* testing *)

open NT;;

(* test_string (make_spaced (word("moshe"))) "   moshe   ";; *)
(* test_string nt_hex "    0x35";; *)
(* test_string nt_boolean "         #t";; *)
(* test_string nt_int "-0";; *)
(* test_string (char ' ') "      p";; *)
(* test_string nt_number "     -0998.0      ";; *)
(* test_string nt_symbol "      aBc 9090";; *)
(* test_string nt_symbol "a";; *)
(* test_string nt_string "   \"  blabla \\t \\\" \" ";; *)

(* test_string nt_sexpr "235";;  *)

test_string nt_sexpr "#2r+1101";;     

