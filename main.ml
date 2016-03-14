open Lexer
open Parser
open Data
open Skolem
open ToSQL

let main ()=
    let lexbuf = Lexing.from_channel stdin in
	let parse () = Parser.main Lexer.token lexbuf in
    let data = parse () in
    Printf.printf "############ 0 pass ############\n";
    print_data data;
    Printf.printf "############ 1 pass ############\n";
    let data = skolemization data in
    print_data data;
    Printf.printf "############ 2 pass ############\n";
    let data = pass data in
    print_data data
;;

main ()
