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

          | Eq
          | NotEq
          | Lt
          | LtEq
          | Gt
          | GtEq

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

(* A map from function ID's (strings) to their implementation programs *)
module IDMap = Map.Make (String) 
type state = program IDMap.t

(* exmaple of using the map *)
(*let get_word_def wName (s : state) =*)
    (*let open IDMap in*)
        (*find_opt wName s;;*)

(* A program needs a state, a stack and the program (word list) *)
type config = (state * stack * program);;

(* we should print these things *)
(* but all these functions need to be mutually recursive *)
let rec print_word w =
        match w with
        | Value v         -> print_value v

        | NamedFunction f -> print_string f

        | Plus            -> print_string "+"
        | Minus           -> print_string "-"
        | Multiplication  -> print_string "*"
        | Division        -> print_string "/"
        | Modulo          -> print_string "%"

        | Eq              -> print_string "="
        | NotEq           -> print_string "<>"
        | Lt              -> print_string "<"
        | LtEq            -> print_string "<="
        | Gt              -> print_string ">"
        | GtEq            -> print_string ">="

        | And             -> print_string "and"
        | Or              -> print_string "or"
        | Not             -> print_string "not"
        | If              -> print_string "if"

        | Dup             -> print_string "dup"
        | Drop            -> print_string "drop"
        | Dollar          -> print_string "$"

        | Define          -> print_string "define"
        | Dot             -> print_string "."

    and print_value ?(in_stack = false) v =
        match v with
        | IdVal s       -> if not in_stack then print_string "\\ " else (); print_string s
        | IntVal i      -> print_int i
        | BoolVal true  -> print_string "true"
        | BoolVal false -> print_string "false"
        | LambdaVal p   -> print_string "( "     ; print_program p  ; print_string " )"

    and print_program p =
        match p with
        | w1 :: (w2 :: wrest as ws) -> print_word w1         ; print_string " " ; print_program ws
        | wfinal :: []              -> print_word wfinal
        | []                        -> ()

    and print_stack s =
        (* use a recursive helper function so we can wrap it all in { } *)
        let rec go s =  
            match s with
            | v1 :: (v2 :: vrest as vs) -> print_value ~in_stack:true v1 ; print_string ", " ; go vs
            | vfinal :: []              -> print_value ~in_stack:true vfinal
            | []                        -> ()
        in print_string "{"; go (List.rev s); print_string "}"

    and print_config ((_, s, p) : config) =
        print_stack s; print_string " => "; print_program p; print_string " "


(* we need a function to pull values up in our stack *)
(* this pulls the value at index i off and rebuilds the stack with that on top *)
let pull_index i stack =
    (* the "go" helper function returns a pair of the selected element and the list without that element *)
    let rec go i stack = 
        match i, stack with
        | 0, cur :: rest -> Some (cur, rest)
        | i, cur :: rest -> let res = go (i-1) rest
                            in Option.bind res (fun (new_head, back) -> Some (new_head, cur :: back))
        | i, [] -> None (* if we run off the list then we have no way to swap *)
    in Option.bind (go i stack) (fun (new_head, rest) -> Some (new_head :: rest))


let step ((sigma, stack, p) : config) = 
    (* program can be a list of statements *)
    match p, stack with
    | [], _ -> None
    (* the first statement can be any defined constructor *)

    (* literals are functions that push themselves on the stack *)
    | Value v :: p', _ -> Some (sigma, v :: stack, p')

    (* Playing around with pattern matching on everything at once *)
    | Plus           :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , IntVal (v1 + v2)   :: s' , p')
    | Minus          :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , IntVal (v1 - v2)   :: s' , p')
    | Multiplication :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , IntVal (v1 * v2)   :: s' , p')
    | Division       :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , IntVal (v1 / v2)   :: s' , p')
    | Modulo         :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , IntVal (v1 mod v2) :: s' , p')

    | Eq    :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , BoolVal (v1 = v2)  :: s' , p')
    | NotEq :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , BoolVal (v1 <> v2) :: s' , p')
    | Lt    :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , BoolVal (v1 < v2)  :: s' , p')
    | LtEq  :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , BoolVal (v1 <= v2) :: s' , p')
    | Gt    :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , BoolVal (v1 > v2)  :: s' , p')
    | GtEq  :: p' , IntVal v2 :: IntVal v1 :: s' -> Some (sigma , BoolVal (v1 >= v2) :: s' , p')

    | And :: p' , BoolVal v2 :: BoolVal v1 :: s' -> Some (sigma , BoolVal (v1 && v2) :: s' , p')
    | Or  :: p' , BoolVal v2 :: BoolVal v1 :: s' -> Some (sigma , BoolVal (v1 || v2) :: s' , p')

    | Not :: p' , BoolVal b :: s'  -> Some (sigma , BoolVal (not b) :: s' , p')

    | If :: p', LambdaVal pFalse :: LambdaVal pTrue :: BoolVal true :: s' -> Some (sigma, s', pTrue @ p')
    | If :: p', LambdaVal pFalse :: LambdaVal pTrue :: BoolVal false :: s' -> Some (sigma, s', pFalse @ p')

    (* Just using a variable instead of the constructor just gives the 1st element, regardless of constructor! *)
    | Dup :: p', v :: s' -> Some (sigma, v :: v :: s', p')
    | Drop :: p', v :: s' -> Some (sigma, s', p')

    
    | Dollar :: p', IntVal index :: s' -> Option.map (fun new_stack -> (sigma, new_stack, p')) (pull_index index s')

    (* definitions! *)
    | Define :: p', LambdaVal fun_imp :: IdVal name :: s' -> Some (IDMap.add name fun_imp sigma, s', p')
    | NamedFunction name :: p', _ -> Option.map (fun fun_imp -> (sigma, stack, fun_imp @ p')) (IDMap.find_opt name sigma)

    (* print to console *)
    | Dot :: p', v :: s' -> print_value v; Some (sigma, s', p')

    | _, _ -> None


let rec run (c : config) =
    match step c with
    | Some c' -> run c'
    | None -> c

let rec walk (c : config) =
    print_config c;
    flush stdout;
    ignore (input_line stdin);
    match step c with
    | Some c' -> walk c'
    | None -> c

let run_program (p : program) = run (IDMap.empty, [], p)

let walk_program (p : program) = walk (IDMap.empty, [], p)

(* added so I can split this up into functions *)
let ap = List.append
let swap : program = [
    Value (IdVal "swap");
        Value (LambdaVal [Value (IntVal 1); Dollar]);
    Define]

(* Something like ... *)
(* 0 1 => 1 0 => 1 0 0 => 0 0 1 => 0 0 1 1 => 0 1 1 0 => 0 1 0 1 *)
let dup2: program = swap @ [
    Value (IdVal "dup2");
        Value (LambdaVal [NamedFunction "swap"; Dup; Value (IntVal 2); Dollar; Dup; Value (IntVal 3); Dollar; NamedFunction "swap"]);
    Define]

(* an infinite loop that fills the stack with the fibonachi sequence *)
let fib: program = dup2 @ [
    Value (IdVal "fib");
        Value (LambdaVal [NamedFunction "dup2"; Plus; NamedFunction "fib"]);
    Define;
    Value (IntVal 1); Dup; NamedFunction "fib"]
