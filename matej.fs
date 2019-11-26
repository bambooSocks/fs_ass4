
type T<'a> = N of 'a * T<'a> list

let td = N("g", [])
let tc = N("c", [N("d",[]); N("e",[td])])
let tb = N("b", [N("c",[])])
let ta = N("a", [tb; tc; N("f",[])])

//ex1
let rec toList = function
    | N (x, []) -> [x]
    | N (x, xs) -> x::auxToList xs
and auxToList = function
    | []        -> []
    | x::xs     -> (toList x) @ (auxToList xs) 

toList ta

//ex2  
let rec map f = function
    | N (x, []) -> N (f x, [])
    | N (x, xs) -> N (f x, auxMap f xs)
and auxMap f = function
    | []        -> []
    | x::xs     -> (map f x)::(auxMap f xs) 
    
map (fun x -> "("+x+")") ta

//ex3
type Path = int list
    
let rec getByIdx i = function
    | []    -> None
    | x::xs -> if i = 0 then Some x else getByIdx (i-1) xs


let rec isPath is t =
    match is with
    | []     -> true
    | i::is' -> match t with
                | N (_, []) -> false
                | N (_, ts) -> let opt = getByIdx i ts
                               match opt with
                               | None    -> false
                               | Some t' -> isPath is' t'

isPath [1;1;0] ta
//ex4
let rec get is t =
    match is with
    | []     -> t
    | i::is' -> match t with
                | N (_, []) -> failwith "out of scope"
                | N (_, ts) -> let opt = getByIdx i ts
                               match opt with
                               | None    -> failwith "wrong path"
                               | Some t' -> get is' t'

get [1;1] ta

// ex5
let rec tryFindPathTo v = function
    | N (v', []) -> if v' = v then Some [] else None
    | N (v', ts) -> if v' = v then Some [] else
                    auxTryFindPathTo v 0 ts
and auxTryFindPathTo v acc = function
    | []    -> None
    | t::ts -> let opt = tryFindPathTo v t
               match opt with
               | None    -> auxTryFindPathTo v (acc+1) ts
               | Some is -> Some (acc::is)
    
tryFindPathTo "g" tb
