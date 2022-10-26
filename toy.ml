
(*A word is either user defined by an id string or a special word*)
type word = 
          | Identifier of string
          | IntLit of int
          | BoolLit of bool
          | Lambda of word list
          | NamedFunction of string

          | Plus
          | Minus
          | Multiplication
          | Division
          | Modulo

          | And
          | Or
          | Not

          | If

          | Dup
          | Drop
          | Dollar
          
          | Define


(*type values = Identifier of string*)
            (*| Word of word*)
            (*| IntLit of int*)
            (*| BoolLit of bool*)
            (*(* NOTE: how do we store functions? *)*)
            (*| Function of values list*)

type program = word list


(*right associative stack*)
type stack' = Empty 
           | Elem of stack' * word

let pop2 (s : stack') =
    match s with
    | Elem (Elem (s', lhs), rhs) -> (lhs, rhs)



type stack = word list

module IDMap = Map.Make (String) 
type state = program IDMap.t

let get_word_def wName (s : state) =
    let open IDMap in
        find_opt wName s;;




