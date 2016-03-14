open Data

(* ############# *)
(* Skolemization *)
(* ############# *)

(* Find the variables of a list of values *)
let rec find_bound_att bound att=
    match att with
    | [] -> bound
    | Val(Variable(name)) :: q -> name :: find_bound_att bound q
    | t::q -> find_bound_att bound att

(* Find the variables of a query *)
let rec find_bound bound q1=
    match q1 with
    | [] -> bound
    | Atom(name, att, s) :: q ->  find_bound_att (find_bound bound q) att 

(* Change an atom in the skolem format *)
let rec modify ind att bound y=
    match att with
    | [] -> []
    | Val(Variable(a)) :: b when List.mem a bound (* a in bound *)-> Val(Variable(a)) :: modify ind b bound y
    | Val(a) :: b -> Skolem (ind, a, y) :: modify ind b bound y
    | _ -> failwith "pas possible"

(* Do the intersection of two lists of strings *)
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
    
let transf_tgd ind q1 q2=
    (* Find the universal variable in q1 *)
    let bound = find_bound [] q1 in
    (* Find the variable in q2 *)
    let used = find_bound [] q2 in
    (* Find the universal variables used in q2 *)
    let y = intersection bound used in
    let rec aux q =
        match q with
        | [] -> []
        | Atom(name, t, str) :: s -> Atom(name, modify ind t bound y, str) :: aux s
    in
    aux q2
    
let skolemization data=
    (* Tansform the tgds *)
    let rec aux ind tgds=
        match tgds with
        | [] -> []
        | Tgd(q1, q2):: s -> Tgd(q1, (transf_tgd ind q1 q2)) :: aux (ind +1) s
    in
    match data with
    | STM ( s, t, Mappings tgds) ->  STM ( s, t, Mappings (aux 1 tgds))
