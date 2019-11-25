open System


type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list

let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb1 = [("Joe", "June Fishing", 28); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb2 = [("Joe", "June Fishing", 28)];;

//Exercise 1
let rec inv sb = match sb with
                  | [] -> true
                  | [(_,_,s)] when s>=0 -> true
                  | [(_,_,s)] -> false
                  | (_,_,s1)::(_,_,s2)::xs -> (List.forall (fun (_,_,x) -> x>0) sb) && (s1>= s2) && (inv xs) 
                  
let test1 = inv sb2

//Exercise 2
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

let test0 = insert ("Martin","March witchHunt",82) sb 
//Exercise 3 

let rec get (n,sb) = match sb with 
                     | [] -> []
                     | [(n1,e1,s1)] when n1 = n -> [(e1,s1)]
                     | [(n1,e1,s1)] -> []
                     | (n1,e1,s1)::xs when n1 = n -> (e1,s1) :: get (n,xs)
                     | _::xs -> get (n,xs)

let test3 = get ("Martin",sb)


//Exercise 4 

let rec top k sb = if k < 0 || List.length sb < k then None else Some (List.take k sb  )
                   
let test4 = top 0 sb 




[<EntryPoint>]
let main argv =
    printfn "hey is %A" test4
    0 // return an integer exit code