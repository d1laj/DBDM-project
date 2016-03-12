open Data

type t_map_couples=
    | Null
    | R of string * string * string * int

type t_map=
    t_map_couples list

let rec find_if_in var map=
    match map with
    | [] -> false
    | R(v, n, a, i)::q when v = var-> true
    | t::q -> find_if_in var q
    
let add var name attri ind map=
    match var with
    | Variable s -> R(s, String.capitalize name, attri, ind) :: map
    | _ -> map



let rec find_bound_att bound att=
    match att with
    | [] -> bound
    | Val(Variable(name)) :: q -> name :: find_bound_att bound q
    | t::q -> find_bound_att bound att

let rec find_bound bound q1=
    match q1 with
    | [] -> bound
    | Atom(name, att) :: q ->  find_bound_att (find_bound bound q) att 

let rec modify ind att bound y=
    match att with
    | [] -> []
    | Val(Variable(a)) :: b when List.mem a bound -> Val(Variable(a)) :: modify ind b bound y
    | Val(a) :: b -> Skolem (ind, a, y) :: modify ind b bound y
    | _ -> failwith "pas possible"

let rec intersection a b=
    let rec aux x b=
        match b with
        | [] -> false
        | t :: q when t = x -> true
        | t :: q -> aux x q
    in
    match a with
    | [] -> []
    | t :: q when aux t b ->  Variable(t) :: intersection q b
    | t :: q -> intersection q b
    
let first_pass ind q1 q2=
    let bound = find_bound [] q1 in
    let used = find_bound [] q2 in
    let y = intersection bound used in
    let rec aux q =
        match q with
        | [] -> []
        | Atom(name, t) :: s -> Atom(name, modify ind t bound y) :: aux s
    in
    aux q2
    
let global_first_pass data=
    let rec aux ind tgds=
        match tgds with
        | [] -> []
        | Tgd(q1, q2):: s -> Tgd(q1, (first_pass ind q1 q2)) :: aux (ind +1) s
    in
    match data with
    | STM ( s, t, Mappings tgds) ->  STM ( s, t, Mappings (aux 1 tgds))

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

let rec construct_map_in_att r name map att ind=
    match att with
    | [] -> map
    | Val(Variable(n)) :: q when find_if_in n map = false -> construct_map_in_att r name (add (Variable n) name (get_attr name ind r) ind map) q (ind +1)
    | Skolem (i, Variable n , l) :: q when find_if_in n map = false -> construct_map_in_att r name (add (Variable n) name (get_attr name ind r) ind map) q (ind +1)
    | t :: q -> construct_map_in_att r name map q (ind +1)


let rec construct_map r map q1 =
    match q1 with
    | [] -> map
    | Atom(name, att) :: q ->  construct_map r (construct_map_in_att r name map att 1) q

let rec rename map q=
    let rec label_of m x =
        match m with
        | [] -> failwith "No label"
        | R(s1, s2, s3, i) :: q when s1 = x -> String.concat "." [s2;s3]
        | t :: q -> label_of q x
    in
    let rec rename_att l=
        match l with
        | [] -> []
        | Val( Variable x) :: q -> Val(Label (label_of map x)) :: rename_att q
        | Skolem (i, Variable x , l) :: q -> Skolem (i, Label (label_of map x) , l) :: rename_att q
        | t :: q -> rename_att q
    in
    match q with
    | [] -> []
    |Atom(name, att) :: tail -> Atom(name, rename_att att) :: rename map tail
    
let second_pass data=
    let rec aux s t tgds=
        match tgds with
        | [] -> []
        | Tgd(q1, q2):: tail -> 
            let map = construct_map t (construct_map s [] q1) q2 in
            Tgd(rename map q1, rename map q2) :: aux s t tail
    in
    match data with
    | STM ( Sources s, Targets t, Mappings tgds) ->  STM ( Sources s, Targets t,  Mappings (aux s t tgds))





