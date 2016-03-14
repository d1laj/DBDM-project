open Lexer
open Parser
open Data
open Skolem
open ToSQL

let main ()=
    let op = ref false in
    let out = ref stdout in
    for i=1 to n-1 do
        match Sys.argv.(i) with
        | "-sqlite3" -> op := true
        | s -> out := open_out s
    done;
    let lexbuf = Lexing.from_channel stdin in
	let parse () = Parser.main Lexer.token lexbuf in
    let data = parse () in
    let data = skolemization data in
    let data = pass data !out in
    let n= Array.length Sys.argv in 
    if !op then begin
        close_out !out;
        
    end
    
;;

main ()
