
(*A word is either user defined by an id string or a special word*)
(* NOTE: We may need to move NamedFunction out to distinguish values *)
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

          (*| Dot (* print out to console if needed *) *)

          (*| Skip (* for small step; we may not actually need this *)*)


(*type values = Identifier of string*)
            (*| Word of word*)
            (*| IntLit of int*)
            (*| BoolLit of bool*)
            (*(* NOTE: how do we store functions? *)*)
            (*| Function of values list*)

type program = word list


(*right associative stack*)
(*type stack' = Empty *)
           (*| Elem of stack' * word*)

(*let pop2 (s : stack') =*)
    (*match s with*)
    (*| Elem (Elem (s', lhs), rhs) -> (lhs, rhs)*)



type stack = word list

module IDMap = Map.Make (String) 
type state = program IDMap.t

let get_word_def wName (s : state) =
    let open IDMap in
        find_opt wName s;;

(* A program needs a state, a stack and the program (word list) *)
type config = (state * stack * program);;


let step ((sigma, stack, p) : config) = 
    (* program can be a list of statements *)
    match p with
    | [] -> None
    (* the first statement can be any defined constructor *)
    (*| IntLit n :: p' -> Some (sigma, IntLit n :: stack, p')*)
    (*| Identifier id :: p' -> Some (sigma, Identifier id :: stack, p')*)
    (* fun with or patterns *)
    | (Identifier _ as x) :: p'
    | (IntLit     _ as x) :: p'
    | (BoolLit    _ as x) :: p'
    | (Lambda     _ as x) :: p' -> Some (sigma, x :: stack, p')



let rec run (c : config) =
    match step c with
    | Some c' -> run c'
    | None -> c

let run_program (p : program) = run (IDMap.empty, [], p)
    



