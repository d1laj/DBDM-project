open Data

type t_map_couples=
    | Null
    | R of t_label * string * string 

type t_map=
    t_map_couples list

let rec find var map=
    match map with
    | [] -> Null
    | R(v, n, a)::q when v = var->  R(v, n, a)
    | t::q -> find var q
    
let add var name attri map=
    R(var, name, attri) :: map



let rec find_bound_att bound att=
    match att with
    | [] -> bound
    | Val(Variable(name)) :: q -> find_bound_att (name :: bound) q
    | t::q -> find_bound_att bound att

let rec find_bound bound q1=
    match q1 with
    | [] -> bound
    | Atom(name, att) :: q -> find_bound (find_bound_att bound att) q

let rec modify ind att bound=
    match att with
    | [] -> []
    | Val(Variable(a)) :: b when List.mem a bound -> Val(Variable(a)) :: modify ind b bound
    | Val(a) :: b -> Skolem (ind, a, []) :: modify ind b bound
    | _ -> failwith "pas possible"

let first_pass ind q1 q2=
    let bound = find_bound [] q1 in
    let rec aux q =
        match q with
        | [] -> []
        | Atom(name, t) :: s -> Atom(name, modify ind t bound) :: aux s
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














