
(*Values can be either ints, identifiers, bools, or functions*)
type value =
          | IdVal of string
          | IntVal of int
          | BoolVal of bool
          | LambdaVal of word list

(*A word is either a function, a pre-defined function, or a literal value *)
  and word = 
          Value of value

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
          | Dot (* print out to console *) 

          (*| Skip (* for small step; we may not actually need this *)*)

(* aliases for our program types *)
type stack = value list
type program = word list

(*right associative stack*)
(*type stack' = Empty *)
           (*| Elem of stack' * word*)

(*let pop2 (s : stack') =*)
    (*match s with*)
    (*| Elem (Elem (s', lhs), rhs) -> (lhs, rhs)*)


module IDMap = Map.Make (String) 
type state = program IDMap.t

let get_word_def wName (s : state) =
    let open IDMap in
        find_opt wName s;;

(* A program needs a state, a stack and the program (word list) *)
type config = (state * stack * program);;


let step ((sigma, stack, p) : config) = 
    (* program can be a list of statements *)
    match p, stack with
    | [], _ -> None
    (* the first statement can be any defined constructor *)

    (* literals are functions that push themselves on the stack *)
    | Value v :: p', _ -> Some (sigma, v :: stack, p')

    (* Playing around with pattern matching on everything at once *)
    | Plus :: p', IntVal v2 :: IntVal v1 :: s' -> Some (sigma, IntVal (v1 + v2) :: s', p')


let rec run (c : config) =
    match step c with
    | Some c' -> run c'
    | None -> c

let run_program (p : program) = run (IDMap.empty, [], p)

