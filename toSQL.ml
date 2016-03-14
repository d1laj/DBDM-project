open Data

type t_map_couples=
    | R of string * ((string * string * string * int) list) (* var name, relation alias, relation name, attribute name, attribute index *)

type t_map=
    t_map_couples list

let rec find_if_in var map=
    match map with
    | [] -> false
    | R(v, l) ::q when v = var-> true
    | t::q -> find_if_in var q
    
let add var alias name attri ind map=
    let rec aux s m=
        match m with
        | [] -> [R(s, [(alias, name, attri, ind)])]
        | R(s1, l) :: q when s = s1 -> R(s1, (alias, name, attri, ind) :: l) :: q
        | t :: q -> t :: aux s q 
    in
    match var with
    | Variable s ->  aux s map
    | _ -> map

let rec print_map m=
    let rec aux l=
        match l with
        |[] -> ()
        |(alias, name, attri, ind) :: q -> Printf.printf "\t%s, %s, %s, %d\n" alias name attri ind; aux q
    in
    match m with
    | [] -> ()
    | R(s1, l) :: q -> Printf.printf "%s\n" s1; aux l; print_map q
    
(* Give aliases to each source *)
let first_pass q1 =
    let rec aux ind q=
        match q with
        |[] -> []
        |Atom(name, att, s) :: r -> Atom(name, att, Printf.sprintf "R%d" ind):: aux (ind + 1) r
    in
    aux 1 q1

(* Get the attribute liked to a relation name and an index *)    
let get_attr relationName ind l=
    let rec elemlist l i=
        match l, i with
        | [] , _ -> failwith "Not enough attributs"
        | t::q, 1 -> t
        | t::q, _ -> elemlist q (i-1)
    in
    let rec aux s=
        match s with
        | [] -> failwith "Not found"
        | Relation(name, attr) :: q when name = relationName-> elemlist attr ind
        | t :: q -> aux q
    in
    aux l

(* Given a set of relations, an alias, a relation name, a map, a list of values and an index; Add the variables to the map *)
let rec construct_map_in_att r alias name map att ind=
    match att with
    | [] -> map
    | Val(Variable(n)) :: q -> construct_map_in_att r alias name (add (Variable n) alias name (get_attr name ind r) ind map) q (ind +1)
   (* | Skolem (i, Variable n , l) :: q -> construct_map_in_att r name (add (Variable n) alias name (get_attr name ind r) ind map) q (ind +1) *)
    | t :: q -> construct_map_in_att r alias name map q (ind +1)


let rec construct_map r map q1 =
    match q1 with
    | [] -> map
    | Atom(name, att, s) :: q ->  construct_map r (construct_map_in_att r s name map att 1) q

let rec rename map q=
    let rec label_of m x =
        match m with
        | [] -> x
        | R(s1, []) :: q -> x
        | R(s1, (s2, s3, s4, i) :: _) :: q when s1 = x -> String.concat "." [s2;s4]
        | t :: q -> label_of q x
    in
    let rec rename_val l =
        match l with
        |[] -> []
        | Variable(x) :: q -> Label (label_of map x) :: rename_val q
        | t :: q -> t:: rename_val q
    in
    let rec rename_att l=
        match l with
        | [] -> []
        | Val( Variable x) :: q -> Val(Label (label_of map x)) :: rename_att q
        | Skolem (i, Variable x , l) :: q -> Skolem (i, Label (label_of map x) , rename_val l) :: rename_att q
        | t :: q -> rename_att q
    in
    match q with
    | [] -> []
    | Atom(name, att, s) :: tail -> Atom(name, rename_att att, s) :: rename map tail

let rec output tgd t map outchan=
    let compute_from q1 map=
        let rec aux q=
            match q with
            | [] -> Printf.fprintf outchan "\n"
            | [Atom(name, att, s)] -> Printf.fprintf outchan "%s AS %s\n" name s
            | Atom(name, att, s) :: r -> Printf.fprintf outchan "%s AS %s, " name s; aux r
        in
        Printf.fprintf outchan "FROM "; aux q1
    in
    let compute_where map=
        let auxtiers a b=
            match a, b with
            | (a1, n1, at1, i1), (a2, n2, at2, i2) -> Printf.fprintf outchan " AND %s.%s = %s.%s" a1 at1 a2 at2
        in
        let rec auxbis l=
            match l with
            | [] -> ()
            | [x] -> ()
            | a :: b :: q -> auxtiers a b; auxbis (b::q)
        in
        let rec aux m=
            match m with
            | [] -> ()
            | R(s, l) :: q when List.length l > 1 -> auxbis l; aux q
            | R(s, l) :: q -> aux q
        in
        Printf.fprintf outchan "WHERE 1=1"; aux map; Printf.fprintf outchan ";\n\n"
    in
    let compute_insert t atom=
        let rec aux name t =
            match t with
            |[] -> ()
            |Relation(n, att) :: q when n = name ->  Printf.fprintf outchan "INSERT INTO %s(%s)\n" name (String.concat ", " att)
            | a :: q -> aux name q
        in
        match atom with
        | Atom(name, att, s) -> aux name t 
    in
    let compute_select atom=
        let rec print_label l=
            match l with
            | [] -> ()
            | [Variable x] -> failwith "impossible case"
            | [Label x] -> Printf.fprintf outchan "%s" x
            | (Variable x) ::q -> failwith "impossible case"
            | (Label x) :: q -> Printf.fprintf outchan "%s || ',' || " x; print_label q
        in 
        let rec aux att=
            match att with
            | [] -> ()
            | Val(Variable(x)) :: q -> failwith "impossible case"
            | [Const i] -> Printf.fprintf outchan "%d\n" i
            | [Val(Label(x))] -> Printf.fprintf outchan "%s\n" x
            | [Skolem(i, Label(v), l)] -> Printf.fprintf outchan "'f[m%d, %s](' || " i v; print_label l; Printf.fprintf outchan " || ')'\n"
            | [Skolem(i, Variable(v), l)] -> Printf.fprintf outchan "'f[m%d, %s](' || " i v; print_label l; Printf.fprintf outchan " || ')'\n"
            | Const(i) :: q -> Printf.fprintf outchan "%d, " i; aux q
            | Val(Label(x)) :: q -> Printf.fprintf outchan "%s, " x; aux q
            | Skolem(i, Label(v), l) :: q -> Printf.fprintf outchan "'f[m%d, %s](' || " i v; print_label l; Printf.fprintf outchan " || ')', "; aux q
            | Skolem(i, Variable(v), l) :: q -> Printf.fprintf outchan "'f[m%d, %s](' || " i v; print_label l; Printf.fprintf outchan " || ')', "; aux q
        in
        match atom with
        | Atom(name, att, s) -> Printf.fprintf outchan "SELECT "; aux att
    in
    match tgd with
    | Tgd(_, []) -> ()
    | Tgd(q1, atom :: q) -> compute_insert t atom; compute_select atom; compute_from q1 map; compute_where map; output (Tgd(q1,q)) t map outchan
     
let second_pass data outchan=
    let rec aux s t tgds=
        match tgds with
        | [] -> []
        | Tgd(q1, q2):: tail -> 
            let map = construct_map s [] q1 in
            let tgd = Tgd(rename map q1, rename map q2) in
            output tgd t map outchan; tgd :: aux s t tail
    in
    match data with
    | STM ( Sources s, Targets t, Mappings tgds) ->  STM ( Sources s, Targets t,  Mappings (aux s t tgds))

let pass data = 
    let rec aux m=
        match m with
        | [] -> []
        | Tgd(q1, q2) :: r -> Tgd(first_pass q1, q2):: aux r
    in
    match data with
    | STM(Sources s, Targets t, Mappings m) -> second_pass (STM(Sources s, Targets t, Mappings (aux m)))
