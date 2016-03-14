open Lexer
open Parser
open Data
open Skolem
open ToSQL

let main ()=
    let lexbuf = Lexing.from_channel stdin in
	let parse () = Parser.main Lexer.token lexbuf in
    let data = parse () in
    let data = skolemization data in
    let data = pass data stdout in
    ()
;;

main ()
