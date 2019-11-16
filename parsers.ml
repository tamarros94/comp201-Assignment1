#use "pc.ml";;
open PC;;

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

let nt_hex =
  let make_NT_digit ch_from ch_to displacement =
    let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
    let nt = pack nt (let delta = (Char.code ch_from) - displacement in
		      fun ch -> (Char.code ch) - delta) in
    nt in
  let nt = disj (make_nt_digit '0' '9' 0)
		(make_nt_digit 'a' 'f' 10) in
  let nt = disj nt (make_nt_digit 'A' 'F' 10) in
  let nt = plus nt in
  let nt = pack nt (fun digits ->
		    List.fold_left (fun a b -> 16 * a + b) 0 digits) in
  let nt = caten (word_ci "0x") nt in
  let nt = pack nt (function (_0x, e) -> e) in
  make_spaced nt;;
end;; (* end of struct PC *)


(* testing *)
open NT;;

test_string (make_spaced (word("moshe"))) "   moshe   ";;