%{
(* --- préambule: ici du code Caml --- *)

open Data  

(*let max_des_litt= ref 0 ;;*)

%}

%token <int> CONST
%token PARO PARF
%token <string> NAME 
%token SOURCE TARGET MAPPING
%token COMMA ARROW DOT DOLLAR
%token EOF

%nonassoc PARO PARF

%start main        
%type <Data.t_data> main 

%%
    
main:
    SOURCE schema TARGET schema MAPPING tgds EOF        { STM(Sources $2, Targets $4, Mappings $6) }
    
schema:
    relation schema                                     { $1 :: $2 }
    | relation                                          { [$1] }
    
relation:
    NAME PARO atts PARF                                 { Relation($1, $3) }
    
atts:
    NAME COMMA atts                                     { $1 :: $3 }
    | NAME                                              { [$1] }

tgds:
    tgd                                                 { [$1] }
    | tgd tgds                                          { $1 :: $2 }
    
tgd:
    query ARROW query DOT                               { Tgd($1, $3) }
    
query:
    atom COMMA query                                    { $1 :: $3 }
    | atom                                              { [$1] }
    
atom:
    NAME PARO args PARF                                 { Atom($1, $3, "") }
    
args:
    value COMMA args                                    { $1 :: $3 }
    | value                                             { [$1] }
    
value:
    variable                                            { Val($1) }
    | CONST                                             { Const $1 }
    
variable:
    DOLLAR NAME                                         { Variable($2) }
