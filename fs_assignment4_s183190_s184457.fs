// Fourth Assignemnt for course 02157 Functional programming
// Mihaela-Elena Nistor - s183190, Matej Majtan - s184457

// Exercise 1

type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list

let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb1 = [("Joe", "June Fishing", 28); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb2 = [("Joe", "June Fishing", 28)];;

// ex 1.1
let rec inv sb = match sb with
                  | [] -> true
                  | [(_,_,s)] when s>=0 -> true
                  | [(_,_,s)] -> false
                  | (_,_,s1)::(_,_,s2)::xs -> (List.forall (fun (_,_,x) -> x>0) sb) && (s1>= s2) && (inv xs) 
                  
let test1 = inv sb2

// ex 1.2
let rec insertHelper (n,e,p) sb = match sb with (*it is assumed that p is positive*)
                                  | [] -> [(n,e,p)] 
                                  | _  ->  try 
                                               let idx = List.findIndex (fun (_,_,pts) -> p>= pts) sb  // finds the index where p >= pts
                                               let first ,second = List.splitAt idx sb // splits into two lists at idx 
                                               let sbNew = first @ [(n,e,p)] @ second //appends the two lists back and in between them adds (n,e,p)
                                               if inv sbNew then sbNew else []
                                            with
                                                //findIndex failed because
                                                //score is the smallest element in sb and it did not find an index
                                                | ex -> sb @ [(n,e,p)]

let rec insert (n,e,p) sb = if p>= 0 then insertHelper (n,e,p) sb else []

let test2 = insert ("Martin","March witchHunt",82) sb 

// ex 1.3 
let rec get (n,sb) = match sb with 
                     | [] -> []
                     | [(n1,e1,s1)] when n1 = n -> [(e1,s1)]
                     | [(n1,e1,s1)] -> []
                     | (n1,e1,s1)::xs when n1 = n -> (e1,s1) :: get (n,xs)
                     | _::xs -> get (n,xs)

let test3 = get ("Martin",sb)

// ex 1.4 
let rec top k sb = if k < 0 || List.length sb < k then None else Some (List.take k sb  )
                   
let test4 = top 0 sb 

// Exercise 2

type T<'a> = N of 'a * T<'a> list

let td = N("g", [])
let tc = N("c", [N("d",[]); N("e",[td])])
let tb = N("b", [N("c",[])])
let ta = N("a", [tb; tc; N("f",[])])

// ex 2.1
let rec toList = function
    | N (x, []) -> [x]
    | N (x, xs) -> x::auxToList xs
and auxToList = function
    | []        -> []
    | x::xs     -> (toList x) @ (auxToList xs) 

let test5 = toList ta

// ex 2.2
let rec map f = function
    | N (x, []) -> N (f x, [])
    | N (x, xs) -> N (f x, auxMap f xs)
and auxMap f = function
    | []        -> []
    | x::xs     -> (map f x)::(auxMap f xs) 
    
let test6 = map (fun x -> "("+x+")") ta

// ex 2.3
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

let test7 = isPath [1;1;0] ta

// ex 2.4
let rec get is t =
    match is with
    | []     -> t
    | i::is' -> match t with
                | N (_, []) -> failwith "out of scope"
                | N (_, ts) -> let opt = getByIdx i ts
                               match opt with
                               | None    -> failwith "wrong path"
                               | Some t' -> get is' t'

let test8 = get [1;1] ta

// ex 2.5
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
    
let test9 = tryFindPathTo "g" tb
