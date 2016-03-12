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
%type <Data.data> main 

%%
    
main:
    SOURCE schema TARGET schema MAPPING tgds EOF        { Data(Sources(&2), Targets(&4), Mappings(&6)) }
    
schema:
    relation schema                                     { $1 :: $2 }
    | relation                                          { [$1] }
    
relation:
    NAME PARO atts PARF                                 { Relation($1, Atts($2)) }
    
atts:
    NAME COMMA atts                                     { $1 :: $3 }
    | NAME                                              { [$1] }

tgds:
    tgd                                                 { [$1] }
    | tgds tgd                                          { $2 :: $1 }
    
tgd:
    query ARROW query DOT                               { Tgd(Query($1), Query($3)) }
    
query:
    atom COMMA query                                    { $1 :: $3 }
    | atom                                              { [$1] }
    
atom:
    NAME PARO args PARF                                 { Atom($1, Args($3)) }
    
args:
    value COMMA args                                    { $1 :: $3 }
    | value                                             { [$1] }
    
value:
    variable                                            { $1 }
    | CONST                                             { $1 }
    
variable:
    DOLLAR NAME                                         { Variable($2) }
