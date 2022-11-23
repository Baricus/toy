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


type typ =
      ArrowTy of (typ list * typ list) 
    | IntTy
    | BoolTy
    | IdTy

type ty_context = typ IDMap.t

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
let pull_index i stack : 'a list option =
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

    | If :: p' , LambdaVal pFalse :: LambdaVal pTrue :: BoolVal true  :: s' -> Some (sigma , s' , pTrue @ p')
    | If :: p' , LambdaVal pFalse :: LambdaVal pTrue :: BoolVal false :: s' -> Some (sigma , s' , pFalse @ p')

    (* Just using a variable instead of the constructor just gives the 1st element, regardless of constructor! *)
    | Dup  :: p' , v :: s' -> Some (sigma , v  :: v   :: s' , p')
    | Drop :: p' , v :: s' -> Some (sigma , s' ,  p')
    
    (* takes a stack index :: rest and returns rest[index] :: rest / rest[index] (rest without the index) *)
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

let run_program (p : program) = let res = run (IDMap.empty, [], p) in print_newline (); res

let walk_program (p : program) = walk (IDMap.empty, [], p)

(* added so I can split this up into functions *)
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

(* factorial the function *)
let fact: program = [
    Value (IdVal "fact");
    Value (LambdaVal [
        Dup; Value (IntVal 1); Gt; (* test for base case *)
        Value (LambdaVal [
            Dup; Value (IntVal 1); Minus; NamedFunction "fact"; Multiplication]); (* True (recursive) case *)
        Value (LambdaVal [
            Dup; Multiplication]); (* False (base) case *)
    If]);
    Define;
    ]

let fact_acc: program = swap @ [
    Value (IdVal "fact_internal");
    Value (LambdaVal [
        Dup; Value (IntVal 1); Gt; (* test for base case *)
        Value (LambdaVal [ (* True (recursive) case *)
            Dup; 
            Value (IntVal 2); Dollar; (* pull accumulator to top *)
            Multiplication; NamedFunction "swap";
            Value (IntVal 1); Minus;
            NamedFunction "fact_internal"]); 
        Value (LambdaVal [ (* False (base) case *)
            Drop; (* remove counter; expose accumlator *)
            Value (IntVal 1); Multiplication]);
        If
    ]);
    Define;
    Value (IdVal "fact");
    Value (LambdaVal [
        Value (IntVal 1); NamedFunction "swap"; (* setup accumulator *)
        NamedFunction "fact_internal"
    ]);
    Define
    ]

(* we should have a really simple parser for this *)
open Option
(* expects a list of strings and pulls out balanced parenthesis *)
(* NOTE: assumes the first ( is not present! I.e. ["Hi"; "there"; ")"]; returns Some (["Hi"; "there"], []) *)
let rec split_parens lst =
    match lst with
    | ")" :: l -> Some ([], l)
    | "(" :: l -> bind (split_parens l) (fun (inner, rest) -> 
            (* have to rewrap the parens to allow for re-parsing later *)
            bind (split_parens rest) (fun (outer, rest') -> Some ("(" :: inner @ ")" :: outer, rest')))
    | any :: l -> bind (split_parens l) (fun (inner, rest) -> Some (any :: inner, rest))
    | [] -> None


let rec take_till_end_annote l
    match l with
    | "]" :: l' -> ([], l')
    | 

let rec parse_ty_list l =
    match l with
    | "Int"  :: l' -> IntTy  :: grab_type_list l'
    | "Bool" :: l' -> BoolTy :: grab_type_list l'
    | "Id"   :: l' -> IdTy   :: grab_type_list l'
    | "["    :: l' -> grab_type_annot l'

let grab_type_annot lst =
    let annot = 
    let intys, outtys = split_arrow
    in ArrowTy (parse_ty_list intys, parse_ty_list outtys)


let rec grab_type_list lst =
    match lst with
    | "Int"  :: l -> IntTy  :: grab_type_list l
    | "Bool" :: l -> BoolTy :: grab_type_list l
    | "Id"   :: l -> IdTy   :: grab_type_list l
    | "->" :: l -> 


let parse s : program option =
    let num_reg = Str.regexp "-?[0-9]+" in
    let rec go words = 
        match words with
        | [] -> Some []

        (* all the easy cases (keywords) *)
        | "+"      :: wds -> bind (go wds) (fun rest -> Some (Plus                  :: rest))
        | "-"      :: wds -> bind (go wds) (fun rest -> Some (Minus                 :: rest))
        | "*"      :: wds -> bind (go wds) (fun rest -> Some (Multiplication        :: rest))
        | "/"      :: wds -> bind (go wds) (fun rest -> Some (Division              :: rest))
        | "%"      :: wds -> bind (go wds) (fun rest -> Some (Modulo                :: rest))
        | "="      :: wds -> bind (go wds) (fun rest -> Some (Eq                    :: rest))

        | "<>"     :: wds -> bind (go wds) (fun rest -> Some (NotEq                 :: rest))
        | "<"      :: wds -> bind (go wds) (fun rest -> Some (Lt                    :: rest))
        | "<="     :: wds -> bind (go wds) (fun rest -> Some (LtEq                  :: rest))
        | ">"      :: wds -> bind (go wds) (fun rest -> Some (Gt                    :: rest))
        | ">="     :: wds -> bind (go wds) (fun rest -> Some (GtEq                  :: rest))

        | "and"    :: wds -> bind (go wds) (fun rest -> Some (And                   :: rest))
        | "or"     :: wds -> bind (go wds) (fun rest -> Some (Or                    :: rest))
        | "not"    :: wds -> bind (go wds) (fun rest -> Some (Not                   :: rest))
        | "if"     :: wds -> bind (go wds) (fun rest -> Some (If                    :: rest))

        | "dup"    :: wds -> bind (go wds) (fun rest -> Some (Dup                   :: rest))
        | "drop"   :: wds -> bind (go wds) (fun rest -> Some (Drop                  :: rest))
        | "$"      :: wds -> bind (go wds) (fun rest -> Some (Dollar                :: rest))

        | "define" :: wds -> bind (go wds) (fun rest -> Some (Define                :: rest))
        | "."      :: wds -> bind (go wds) (fun rest -> Some (Dot                   :: rest))

        | "true"   :: wds -> bind (go wds) (fun rest -> Some (Value (BoolVal true)  :: rest))
        | "false"  :: wds -> bind (go wds) (fun rest -> Some (Value (BoolVal false) :: rest))

        | "\\" :: [] -> None
        (* prevent \ + and things like that  for predefined syntax *)
        | "\\" :: "+"      :: _ -> None
        | "\\" :: "-"      :: _ -> None
        | "\\" :: "*"      :: _ -> None
        | "\\" :: "/"      :: _ -> None
        | "\\" :: "%"      :: _ -> None
        | "\\" :: "="      :: _ -> None

        | "\\" :: "<>"     :: _ -> None
        | "\\" :: "<"      :: _ -> None
        | "\\" :: "<="     :: _ -> None
        | "\\" :: ">"      :: _ -> None
        | "\\" :: ">="     :: _ -> None

        | "\\" :: "and"    :: _ -> None
        | "\\" :: "or"     :: _ -> None
        | "\\" :: "not"    :: _ -> None
        | "\\" :: "if"     :: _ -> None

        | "\\" :: "dup"    :: _ -> None
        | "\\" :: "drop"   :: _ -> None
        | "\\" :: "$"      :: _ -> None

        | "\\" :: "define" :: _ -> None
        | "\\" :: "."      :: _ -> None

        | "\\" :: "true"   :: _ -> None
        | "\\" :: "false"  :: _ -> None
        (* we can't have a number here but anything else is good *)
        | "\\" :: id :: wds -> 
                if not (Str.string_match num_reg id 0)
                    then bind (go wds) (fun rest -> Some (Value (IdVal id) :: rest))
                    else None

        (* lambdas we need to split off everything to the next parens recursively *)
        | ")" :: _ -> None
        | "([" :: wds -> 
                bind (split_parens wds) (fun (inside, outside) ->
                    bind (go inside) (fun prog ->
                        bind (go outside) (fun rest -> Some (Value (LambdaVal prog) :: rest))))

        (* either a number or a named function *)
        | other :: wds -> bind (go wds) (fun rest -> 
                if Str.string_match num_reg other 0
                    then Some (Value (IntVal (int_of_string other)) :: rest)
                    else Some (NamedFunction other :: rest))

    in go (Str.split (Str.regexp "[ \t\n]+") s)


let typecheck (prog : program) (stack : typ list) = 
    let rec typecheck_internal prog input stack =
        match prog with
        | [] -> Some (ArrowTy (input, stack))
        | Plus :: rest | Minus :: rest | Multiplication :: rest | Division :: rest | Modulo :: rest
            -> (match stack with
                | IntTy :: IntTy :: stack -> typecheck_internal rest input (IntTy :: stack)
                | _ -> None)

        | Eq :: rest | Lt :: rest | Gt :: rest | LtEq :: rest | GtEq :: rest | NotEq :: rest
            -> (match stack with
                | IntTy :: IntTy :: stack -> typecheck_internal rest input (BoolTy :: stack)
                | _ -> None)

        | And :: rest | Or :: rest
            -> (match stack with
                | BoolTy :: BoolTy :: stack -> typecheck_internal rest input (BoolTy :: stack)
                | _ -> None)

        | Not :: rest
            -> (match stack with
                | BoolTy :: stack -> typecheck_internal rest input (BoolTy :: stack)
                | _ -> None)

        | Dup :: rest
            -> (match stack with
                | a :: stack -> typecheck_internal rest input (a :: a :: stack)
                | _ -> None)

        | Drop :: rest | Dot :: rest
            -> (match stack with
                | a :: stack -> typecheck_internal rest input stack
                | _ -> None)

        | Value (IntVal n) :: Dollar :: rest
            -> Option.bind (pull_index n stack) (fun stack -> typecheck_internal rest input stack)
        | Dollar :: _ -> None (* prevent dependent type requirements *)

        (* for later ? TODO *)

        | Value (IdVal id) :: Value (LambdaVal impl) :: Define :: rest -> None
        | Define :: _ -> None
        | NamedFunction word :: rest -> None (* lookup in (currently non-existent) gamma *)
        | Value (LambdaVal _) :: rest -> None (* typecheck_internal input function and add arrow to the stack *)
        | If :: rest -> None (* use types of two lamba values *)


        (* base literals *)
        | Value (IntVal _) :: rest -> typecheck_internal rest input (IntTy :: stack)
        | Value (BoolVal _) :: rest -> typecheck_internal rest input (BoolTy :: stack)
        | Value (IdVal _) :: rest -> typecheck_internal rest input (IdTy :: stack)
    in typecheck_internal prog stack stack
    




(* so now we can write an interpreter! *)
(* NOTE: this should typecheck first, once we have that *)
(* TODO: do that /\ *)
(* TODO: modify parser so we can parse types of lambdas 
I was thinking something like making lambdas list the inputs beforehand like:
    ([ int int ] + )
and we can just call "typecheck body [ int ; int ]"
   *)
let interpret ?(walk = false) (s : string) : config option =
    match parse s with
    | None -> print_string "Cannot parse program\n"; None
    | Some program -> Some (if walk then walk_program program else run_program program)

(* and proper programs! *)
let fact_acc_str = "
\\ swap ( 1 $ ) define 
\\ fact_internal ( dup 1 > ( dup 2 $ * swap 1 - fact_internal ) ( drop 1 * ) if ) define 
\\ fact ( 1 swap fact_internal ) define 5 fact"

(* calculates the next leapyear *)
let next_leapyear_str = "
\\ is_leapyear ( dup 4 % ) define
\\ next_leapyear ( is_leapyear 0 = ( 4 + ) ( is_leapyear 4 1 $ - + ) if ) define 
    2021 next_leapyear"

