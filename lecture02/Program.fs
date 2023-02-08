

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

type complex = float * float
type word = (char * int) list
type squareFun = word -> int -> int -> int
type square = (int * squareFun) list
let downto1 (n: int) : int list = if n > 0 then List.init (n) (fun x -> x + 1) |> List .rev else []


let downto2 (n: int) : int list =
    let rec aux n cont =
        match n with
        | x when x > 0 -> aux (x - 1) (fun result ->  cont (x :: result))
        | _ -> cont []
    aux n id
    
    
let rec combinePair (xs: 'a list) : ('a * 'a ) list =
    match xs with
    | x::y::ys -> (x, y):: combinePair (ys)
    | [x] -> []
    | [] -> []
    
let combinePair2 (xs: 'a list) : ('a * 'a ) list =
    let rec aux xs cont = 
        match xs with
        | x::y::ys -> aux (ys) (fun result -> cont ((x, y) :: result))
        | [x] -> cont []
        | [] -> cont []
    aux xs id
    
let removeOddIdx (xs: 'a list) : 'a list =
    List.indexed xs
    |> List.fold (fun acc (k,v) -> if k % 2 = 0 then acc@[v] else acc ) []
let mkComplex (a: float) (b: float) : complex = complex(a, b)
   
let complexToPair (complex: complex) : (float * float) = (fst complex, snd complex)
    
 
   
let (|+|) (a:complex) (b:complex) = (fst a + fst b, snd a + snd b)
let (|*|) (a:complex) (b:complex) = ((fst a * fst b) - (snd a * snd b), snd a * fst b + fst a * snd b)

let (|-|) (a: complex) (b: complex) = (a |+| (-(fst b), -(snd b)))
let (|/|) (a: complex) (b: complex) = a |*| ((fst b / (((fst b ** 2.0)) + (snd b ** 2.0))), (-(snd b) / ((fst b ** 2.0) + (snd b ** 2.0))))
let explode1 (s: string) =
    Seq.toList s
    
let explode2 (s:string) =
    let rec aux str =
        match str with
        | "" -> []
        | x -> (x.Chars 0) :: aux (x.Remove(0, 1))
    aux s 

    

let implode (cs ) : string = List.foldBack (fun x acc -> (string x + acc)) cs ""

let implodeRev (cs ) : string = List.fold (fun acc x -> (string x + acc)) "" cs

let toUpper (s:string) : string = explode1 s |> List.map System.Char.ToUpper |> implode
let toUpperWithComposition = explode1 >> List.map (System.Char.ToUpper) >> implode
let rec ack ((m: int), (n: int))  =
    match m,n with
    | (a, b) when a = 0 -> b + 1
    | (a, b) when a > 0 && b = 0 -> ack (a-1, 1)
    | (a, b) when  a > 0 && b > 0 ->  ack (a-1, ack (a, b - 1))
    | _ -> failwith "Something went wrong with the pattern matching"
    
let time f =
     let start = System.DateTime.Now
     let res = f ()
     let finish = System.DateTime.Now
     (res, finish - start)
let timeArg1 f a = time (fun () -> f a)

//let downto3 f n e = if n > 0 then f n e else e

let rec downto3 f n e = if n > 0 then downto3 f (n-1) (f n e) else e

// fac 3
// downto (f1 * f2 -> f3) (2) 1
// downto (f3 * f4 -> f5) (1) 1
// downto (f5 * f6 -> f7) (0) 1


let fac n = downto3 (*) n 1

let range (g: int -> 'a) (n: int) : 'a list = downto3 (fun x y -> (g x)::y) n []

let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1);]

let singleLetterScore (word: word) (pos:int) (acc: int) : int = snd (word.[pos]) + acc

let doubleLetterScore (word: word) (pos:int) (acc: int) : int = (snd (word.[pos]) * 2) + acc

let tripleLetterScore (word: word) (pos:int) (acc: int) : int = (snd (word.[pos]) * 3) + acc

let doubleWordScore (word: word) (pos:int) (acc: int) : int = acc * 2

let tripleWordScore (word: word) (pos:int) (acc: int) : int = acc * 3

let vowels = ['a'; 'e'; 'i'; 'o'; 'u'; 'A'; 'E'; 'I'; 'O'; 'U']

let isVowel ch = if List.contains ch vowels then true else false

let oddConsonants (word: word) (pos:int) (acc: int) : int =
    if (List.fold (fun acc (ch, _) -> if not (isVowel (ch)) then (acc + 1) else acc) 0 word) % 2 = 0 then acc else -(acc)


// Squares

let SLS : square = [(0, singleLetterScore)];;
let DLS : square = [(0, doubleLetterScore)];;
let TLS : square = [(0, tripleLetterScore)];;
let DWS : square = SLS @ [(1, doubleWordScore)];;
let TWS : square = SLS @ [(1, tripleWordScore)];;

