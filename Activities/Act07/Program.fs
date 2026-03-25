/// KAIST CS220 In-Class Activity Project

/// Problem 1: this is currently an integer list. Modify this type to be a
/// generic list type.
type MyList<'a> =
  | Nil
  | Cons of 'a * MyList<'a>

let empty =
  Nil

let cons elt lst =
  Cons (elt, lst)

let car lst =
  match lst with
  | Nil -> failwith "Empty list"
  | Cons (h, _) -> h

let cdr lst =
  match lst with
  | Nil -> failwith "Empty list"
  | Cons (_, t) -> t

let (^+^) = cons

let intList = 1 ^+^ 2 ^+^ 3 ^+^ empty
let stringList = "a" ^+^ "B" ^+^ "c" ^+^ empty

printfn "%A" <| intList
printfn "%d" <| car intList
printfn "%A" <| cdr intList
printfn "%A" <| stringList
printfn "%A" <| car stringList
printfn "%A" <| cdr stringList

/// Problem 2: this function returns the length of the given list.
let rec length lst =
  match lst with
  | Nil -> 0
  | Cons (_, t) -> 1 + length t

printfn "%d" <| length intList
printfn "%d" <| length stringList

/// Problem 3: this function checks if the given element is included in the
/// given list.
let rec isMember elm lst =
  match lst with
  | Nil -> false
  | Cons (h, t) -> if h = elm then true else isMember elm t

printfn "is X a member? %b" <| isMember "X" stringList
printfn "is B a member? %b" <| isMember "B" stringList

/// Problem 4: this function joins two lists.
let rec append lst1 lst2 =
  match lst1 with
  | Nil -> lst2
  | Cons (h, t) -> Cons (h, append t lst2)

let lst1 = 1 ^+^ 2 ^+^ empty
let lst2 = 3 ^+^ 4 ^+^ empty
printfn "%A" <| append lst1 lst2

/// Problem 5: this function returns a reversed list.
let rev lst =
  let rec loop acc lst =
    match lst with
    | Nil -> acc
    | Cons (h, t) -> loop (Cons (h, acc)) t
  loop Nil lst

printfn "%A" <| rev lst1
