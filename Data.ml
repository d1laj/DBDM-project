

(* Type declaration *)

type t_label=
    | Variable of string
    | Label of string
    
type t_value=
    | Const of int
    | Val of t_label
    | Skolem of int * t_label * (t_label list)
  
type t_atom=
    Atom of string * (t_value list)
    
type t_tgd=
    Tgd of (t_atom list) * (t_atom list)
    
type t_mappings=
    Mappings of t_tgd list
    
type t_relation=
    Relation of string * (string list)
    
type t_sources=
    Sources of t_relation list
    
type t_targets=
    Targets of t_relation list
    
type t_data=
    STM of t_sources * t_targets * t_mappings

(* Data integrity check *)

let check data= match data with
    | Sources s, Targets t, Mappings m ->
        let check_relation rel atom= match rel, atom with
            |Relation(name1, att1), Atom(name2, att2) when name1 = name2 && List.length att1 = List.length att2 -> true
            | _ -> false
        in
        let rec query_check r q=
            match r, q with 
            | _, [] -> true
            | [], a::b -> false
            | rel :: s, [a] when check_relation rel a -> true
            | rel :: s, [a] -> query_check s q
            | rel :: s, a :: b -> query_check r [a] && query_check r b
        in
        let local_check h = 
            match h with
            | Tgd(query1, query2) -> query_check s query1 && query_check t query2
        in
        let rec global_check m= match m with
            | [] -> true
            | h::q -> local_check h && global_check q
        in
        global_check m


let print_data data=
    let print_relation t =
        let rec aux l=
            match l with
            | [] -> Printf.printf ")\n"
            | [t] -> Printf.printf "%s" t; aux []
            | t :: q -> Printf.printf "%s," t; aux q
        in
        match t with
        | Relation (name, liste) ->   Printf.printf "%s(" name;
                                    aux liste;
    in
    let print_sources s=
        Printf.printf "SOURCES\n";
        let rec aux l=
            match l with
            | [] -> ()
            | t :: q -> print_relation t; aux q
        in 
        match s with
        | Sources l -> aux l
    in
    let print_targets t=
        Printf.printf "\nTARGETS\n";
        let rec aux l=
            match l with
            | [] -> ()
            | t :: q -> print_relation t; aux q
        in 
        match t with
        | Targets l -> aux l
    in
    let rec print_label l=
        match l with
        | [] -> ()
        | [Variable x] -> Printf.printf "$%s" x
        | [Label x] -> Printf.printf "%s" x
        | (Variable x) ::q -> Printf.printf "$%s, " x; print_label q
        | (Label x) :: q -> Printf.printf "%s, " x; print_label q
    in    
    let rec print_val l= match l with
        | [] -> ()
        | [Const(i)] -> Printf.printf "%d" i
        | [Val v] -> print_label [v];
        | [Skolem (i, v , l)] -> Printf.printf "f[m%d, " i; print_label [v]; Printf.printf "]("; print_label l; Printf.printf ")"
        | Const(i) :: q -> Printf.printf "%d, " i; print_val q
        | Val v :: q -> print_label [v]; Printf.printf ", "; print_val q
        | Skolem (i, v , l) :: q -> Printf.printf "f[m%d, " i; print_label [v]; Printf.printf "]("; print_label l; Printf.printf "),"; print_val q
    in
    let print_atom x=
        match x with
        | Atom (name, values) -> Printf.printf "%s(" name; print_val values; Printf.printf ")";
    in
    let rec print_atom_list l=
        match l with
        | [] -> ()
        | [x] -> print_atom x
        | t :: q -> print_atom t; Printf.printf " ^ "; print_atom_list q
    in
    let print_mappings m=
        let rec aux l =
            match l with
            | [] -> ()
            | Tgd(t1, t2) :: q -> print_atom_list t1; Printf.printf " -> "; print_atom_list t2; Printf.printf ".\n"; aux q
        in
        match m with
        | Mappings l -> Printf.printf "\nMAPPINGS\n"; aux l
    in  
    match data with
    | STM(s,t,m) -> print_sources s; print_targets t; print_mappings m
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
