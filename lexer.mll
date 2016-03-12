{
open Parser ;;        (* le type "token" est défini dans parser.mli *)
}

rule token = parse 
  | [' ' '\t']          { token lexbuf } 
  | "--"[^'\n']*'\n'    { token lexbuf } 
  | "--\n"           	{ token lexbuf } 
  | '\n'            	{ token lexbuf }
  
  | ','                 { COMMA }
  | "->"                { ARROW }
  | '.'                 { DOT }
  | '$'                 { DOLLAR }
  | eof             	{ EOF }
  
  | '('                 { PARO }
  | ')'                 { PARF }
  
  | "SOURCE"            { SOURCE }
  | "TARGET"            { TARGET }
  | "MAPPING"           { MAPPING }
  
  | ['0'-'9']+ as s     { CONST (int_of_string s) }
  | ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] *  as s   { NAME (s) }
  
  | _ 			{ token lexbuf }




