open System


type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list

let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb1 = [("Joe", "June Fishing", 28); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
let sb2 = [("Joe", "June Fishing", -28)];;
let sb3 = []
//Exercise 1
let rec inv sb = match sb with
                  | [] -> true
                  | [(_,_,s)] when s>=0 -> true
                  | [(_,_,s)] -> false
                  | (_,_,s1)::(_,_,s2)::xs -> (List.forall (fun (_,_,x) -> x>0) sb) && (s1>= s2) && (inv xs) 
// checks if the last branch of the function is executed and returns true          
let test1 = inv sb

//checks if the third branch of the function is executed and returns false since -28 < 0 
let test2 = inv sb2


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

//inserts ("Martin","March Deer Hunt",82) in sb at the beginning of the list since 82 is greater than all the other scores in sb 
//result: [("Martin", "March Deer Hunt", 82); ("Joe", "June Fishing", 35);("Peter", "May Fishing", 30); ("Joe", "May Fishing", 28);("Paul", "June Fishing", 28)]
let test3 = insert ("Martin","March Deer Hunt",82) sb 

//inserts ("Matej","September Beever Hunt",13) in sb at the end of the list since 13 is smaller than any of the scores in sb 
//result : [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28);("Matej", "September Beever Hunt", 13)]
let test4 = insert ("Matej","September Beever Hunt",13) sb

//inserts ("Matej","September Beever Hunt",29) in sb at index 2 
//result [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Matej", "September Beever Hunt", 29); ("Joe", "May Fishing", 28);("Paul", "June Fishing", 28)]
let test5 = insert ("Matej","September Beever Hunt",29) sb 


//Exercise 3 

let rec get (n,sb) = match sb with 
                     | [] -> []
                     | [(n1,e1,s1)] when n1 = n -> [(e1,s1)]
                     | [(_,_,_)] -> []
                     | (n1,e1,s1)::xs when n1 = n -> (e1,s1) :: get (n,xs)
                     | _::xs -> get (n,xs)
//checks if the third branch of the function is executed and returns as result []
let test6 = get ("Martin",sb)

//checks if the 4th branch of the function is executed
//returns [("June Fishing", 35); ("May Fishing", 28)]
let test7 = get ("Joe",sb)

//Exercise 4 

let rec top k sb = if k < 0 || List.length sb < k then None else Some (List.take k sb  )

//returns Some [] since k = 0                    
let test8 = top 0 sb 

//returns the sublist of the first three elements of sb 
//result Some[("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28)]
let test9 = top 3 sb 

//returns None since 10 is greater than the number of elements in sb 
let test10 = top 10 sb 


[<EntryPoint>]
let main argv =
    printfn "hey is %A" test10
    0 // return an integer exit code