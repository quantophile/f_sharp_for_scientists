#light
// These are my F# solutions of Ninety-Nine Haskell problems
// which are themselves translations of Ninety-nine LISP problems.

// Problem 1. Find the last element of the list.

/// Example in F#: 
/// > myLast [1; 2; 3; 4];;
/// val it : int = 4
/// > myLast ['x';'y';'z'];;
/// val it : char = 'z'

let myLast inputList = 
    let rec traverseList args = 
        match args with
        | [] -> failwithf "Empty list"
        | [x] -> x
        | _::t -> traverseList t

    traverseList inputList

myLast [1;2;3;4;5];;

// Problem 2. Find the last but one element of the list.
/// Example in F#: 
/// myButLast [1; 2; 3; 4];;
/// val it : int = 3
/// > myButLast ['a'..'z'];;
/// val it : char = 'y'

let rec myButLast args =
    match args with
    | [] -> failwithf "Empty list!"
    | [x] -> failwithf "List contains only one element"
    | [x;y] -> x
    | h::t -> myButLast t

myButLast [1; 2; 3; 4];;

// Problem 3. Find the k'th element of a list.

/// Example in F#: 
/// > elementAt [1; 2; 3] 2;;
/// val it : int = 2
/// > elementAt (List.ofSeq "fsharp") 5;;
/// val it : char = 'r'

let rec elementAt args k = 
    match args, k with
    | [],_ -> failwithf "End of list reached!"
    | _,0 -> failwithf "k must be greater than 0!"
    | [x],1 -> x
    | h::t,1 -> h
    | h::t,k -> elementAt t (k-1) ;;

elementAt [1; 2; 3] 0;;
elementAt [1; 2; 3] 1;;
elementAt [1; 2; 3] 2;;
elementAt [1; 2; 3] 3;;
elementAt [1; 2; 3] 4;;

// Problem 4. Find the number of elements in a list.
/// 
/// > myLength [123; 456; 789];;
/// val it : int = 3
/// > myLength <| List.ofSeq "Hello, world!"
/// val it : int = 13 

let myLength inputList =
    let rec length args n =
        match args,n with
        | [],n ->  n
        | [x],n -> n+1
        | h::t,n -> length t (n+1)
    length inputList 0  ;;

myLength [123; 456; 789];;
myLength <| List.ofSeq "Hello, world!"

// Problem 5. Reverse a list.
/// val it : char list =
///  ['!'; 'a'; 'm'; 'a'; 'n'; 'a'; 'p'; ' '; ','; 'l'; 'a'; 'n'; 'a'; 'c'; ' ';
///   'a'; ' '; ','; 'n'; 'a'; 'l'; 'p'; ' '; 'a'; ' '; ','; 'n'; 'a'; 'm'; ' ';
///   'A']
/// > reverse [1,2,3,4];;
/// val it : int list = [4; 3; 2; 1]

let reverse inputList = 
    let rec reverseList args out =
        match args with
        | [] -> out
        | [x] -> [x] @ out
        | h::t -> reverseList t [h]@out

    reverseList inputList [];;

reverse <| List.ofSeq ("A man, a plan, a canal, panama!")

// Problem 6. Find out whether a list is a palindrome or not
/// A palindrome can be read forward or backward; e.g. (x a m a x).
/// 
/// Example in F#: 
/// > isPalindrome [1;2;3];;
/// val it : bool = false
/// > isPalindrome <| List.ofSeq "madamimadam";;
/// val it : bool = true
/// > isPalindrome [1;2;4;8;16;8;4;2;1];;
/// val it : bool = true

let isPalindrome args =
    args = reverse args;;

isPalindrome [1;2;3];;
isPalindrome <| List.ofSeq "madamimadam";;

// Problem 7. Flatten a nested list.
/// Transform a list, possibly holding lists as elements into a `flat' list by replacing each 
/// list with its elements (recursively).
///  
/// Example: 
/// * (my-flatten '(a (b (c d) e)))
/// (A B C D E)
///  
/// Example in F#: 
/// 
type 'a NestedList = 
    | List of 'a NestedList list 
    | Elem of 'a

/// > flatten (Elem 5);;
/// val it : int list = [5]
/// > flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]]);;
/// val it : int list = [1;2;3;4;5]
/// > flatten (List [] : int NestedList);;
/// val it : int list = []

let l1 = List [ Elem(1);Elem(2);Elem(3);Elem(4);Elem(5)]

let rec flatten (input:'a NestedList) =
    let rec flattenList (args:'a NestedList list) output = 
        match args with 
        | [] -> output
        | [x] -> output @ flatten x
        | head :: tail -> let firstPiece = flatten head 
                          let secondPiece = flattenList tail []
                          output @ firstPiece @ secondPiece
    match input with
    | List aList -> flattenList aList []
    | Elem anElement -> [anElement];;

flatten l1;;
flatten (Elem 5);;
flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]]);;

// Problem 8. Eliminate consecutive duplicates of of list elements
/// If a list contains repeated elements they should be replaced with a single copy of the 
/// element. The order of the elements should not be changed.
/// Example in F#: 
/// 
/// > compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
/// val it : string list = ["a";"b";"c";"a";"d";"e"]

let rec compress input output =
        match input with 
        | [] -> output
        | [x] -> output @ [x]
        | h::t -> if h = List.head t then
                      compress ([h] @ List.tail t) output
                  else
                      compress t (output @ [h])

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] [];;

// Problem 9. Pack consecutive duplicates of list elements into sublists.
/// If a list contains repeated elements they should be placed 
/// in separate sublists.
// Example in F#: 
/// 
/// > pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 
///         'a'; 'd'; 'e'; 'e'; 'e'; 'e']
/// val it : char list list =
///  [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; ['a'; 'a']; ['d'];
///   ['e'; 'e'; 'e'; 'e']]

let pack (input: 'a list) =
    let rec packAux (args:'a list) (subList: 'a list) output =
        match args with
        | [] -> output @ [subList]
        | [x] -> packAux [] (subList @ [x]) output
        | h::t -> if h = List.head t then
                     packAux t (subList @ [h]) output
                  else
                     packAux t [] (List.append output [subList @ [h]])

    let output = packAux input [] []
    output;;

pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'];;