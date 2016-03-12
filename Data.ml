

(* Type declaration *)

type t_value=
    | int
    | Variable of string

type t_arguments=
    Args of t_value list
    
type t_atom=
    Atome of string * t_arguments
    
type t_query=
    Query of t_atom list
    
type t_tgd=
    Tgd of t_query * t_query
    
type t_mappings=
    Mappings of t_tgd list
    
type t_atts=
    Atts of string list
    
type t_relation=
    Relation of string * t_atts
    
type t_sources=
    Sources of t_relation list
    
type t_targets=
    Targets of t_relation list
    
type data=
    Data of t_sources * t_targets * t_mappings


