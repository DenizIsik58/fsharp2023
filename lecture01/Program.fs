

// For more information see https://aka.ms/fsharp-console-apps
let sqr (x:int) : int = (x * x)
printfn "%A" (sqr 2) // Should return 4

let pow (x:float) (n:float) = (x**n)
printfn "%A" (pow 2 3) // Should return 8

let rec sum (n:int) : int =
    match n with
    | x when x <= 0 -> 0
    | x when x >= 0 -> sum (n-1) + n

printfn "%A" (sum 6) // Should return 21


let rec fib (x:int) : int =
    match x with
    | n when n = 0 -> 0
    | n when n = 1 -> 1
    | n -> (fib (n-1)) + (fib (n-2))
    
printfn "%A" (fib 6)

let dub (s:string) : string = s + s
printfn "%s" (dub "HI ")

let rec dubn (s:string) (n:int) : string = if n = 1 then s else (dubn s (n-1) ) + s
printfn "%s" (dubn "HI " 3)

let rec bin (x: int * int) : int =
    match x with
    | n, k when n = k || k = 0 -> 1
    | n, k when n <> 0 && k <> 0 && n > k -> bin ((n-1),(k-1)) + bin ((n-1), k)
    | _ -> failwith "error"
    
let timediff (t1: int * int) (t2: int * int) =
    match t1, t2 with
    | (a, b), (x, y) when a >= 0 && x >= 0 && a < 24 && x < 24 && b >= 0 && y >= 0 && b < 60 && y < 60 ->
      ((x - a) * 60) + (y - b)
    | _ -> failwith "Please specify an hour in the range 0-23 and minutes in the range of 0-59"

let minutes (time: int * int ) =
    match time with
    | (a, b) when b >= 0 && b < 60 && a < 24 && a > 0 -> ((a * 60) + b)
    | _ -> failwith "error"
    
let curry (f: 'a * 'b -> 'c) (a: 'a) (b: 'b) : 'c = f (a, b) // (fun (a, b) -> (a, b))
let uncurry (f: ('a -> 'b -> 'c) ) ( p: 'a * 'b) : 'c = f (fst p) (snd p) // 

let empty ( letter:char,  pointValue: int) : (int -> char * int) = fun _ ->  (letter, pointValue)

let add (newPos: int) (cv: char * int) (word: (int -> char * int)) : (int -> char * int)= (fun i -> if i = newPos then cv else word (i)) 

let wordHello = empty (char 0, 0) |> add 1 ('H', 4) |> add 2 ('E', 1) |> add 3 ('L', 1) |> add 4 ('L', 1) |> add 5 ('O', 1)

let singleLetterScore (word: int -> char * int) (pos: int) = snd (word (pos))

let doubleLetterScore (word: int -> char * int) (pos: int) = (singleLetterScore word pos) * 2

let trippleLetterScore (word: int -> char * int) (pos: int) = (singleLetterScore word pos) * 3