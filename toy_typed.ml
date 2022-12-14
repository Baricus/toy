type typ =
      ArrowTy of (typ list * typ list) 
    | IntTy
    | BoolTy
    | IdTy

(*Values can be either ints, identifiers, bools, or functions*)
type value =
          | IdVal of string
          | IntVal of int
          | BoolVal of bool
          | LambdaVal of (word list * typ) (* must be an arrowty *)

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
        | IdVal s         -> if not in_stack then print_string "\\ " else (); print_string s
        | IntVal i        -> print_int i
        | BoolVal true    -> print_string "true"
        | BoolVal false   -> print_string "false"
        | LambdaVal (p,_) -> print_string "( "     ; print_program p  ; print_string " )"

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

    | If :: p' , LambdaVal (pFalse,_) :: LambdaVal (pTrue,_) :: BoolVal true  :: s' -> Some (sigma , s' , pTrue @ p')
    | If :: p' , LambdaVal (pFalse,_) :: LambdaVal (pTrue,_) :: BoolVal false :: s' -> Some (sigma , s' , pFalse @ p')

    (* Just using a variable instead of the constructor just gives the 1st element, regardless of constructor! *)
    | Dup  :: p' , v :: s' -> Some (sigma , v  :: v   :: s' , p')
    | Drop :: p' , v :: s' -> Some (sigma , s' ,  p')
    
    (* takes a stack index :: rest and returns rest[index] :: rest / rest[index] (rest without the index) *)
    | Dollar :: p', IntVal index :: s' -> Option.map (fun new_stack -> (sigma, new_stack, p')) (pull_index index s')

    (* definitions! *)
    | Define :: p', LambdaVal (fun_imp,_) :: IdVal name :: s' -> Some (IDMap.add name fun_imp sigma, s', p')
    | NamedFunction name :: p', _ -> Option.map (fun fun_imp -> (sigma, stack, fun_imp @ p')) (IDMap.find_opt name sigma)

    (* print to console *)
    | Dot :: p', v :: s' -> print_value ~in_stack:true v; print_newline (); Some (sigma, s', p')

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
(* NOTE: commented out since this was before type annotations *)
(*let swap : program = [*)
    (*Value (IdVal "swap");*)
        (*Value (LambdaVal [Value (IntVal 1); Dollar]);*)
    (*Define]*)

(* Something like ... *)
(* 0 1 => 1 0 => 1 0 0 => 0 0 1 => 0 0 1 1 => 0 1 1 0 => 0 1 0 1 *)
(*let dup2: program = swap @ [*)
    (*Value (IdVal "dup2");*)
        (*Value (LambdaVal [NamedFunction "swap"; Dup; Value (IntVal 2); Dollar; Dup; Value (IntVal 3); Dollar; NamedFunction "swap"]);*)
    (*Define]*)

(* an infinite loop that fills the stack with the fibonachi sequence *)
(*let fib: program = dup2 @ [*)
    (*Value (IdVal "fib");*)
        (*Value (LambdaVal [NamedFunction "dup2"; Plus; NamedFunction "fib"]);*)
    (*Define;*)
    (*Value (IntVal 1); Dup; NamedFunction "fib"]*)

(* factorial the function *)
(*let fact: program = [*)
    (*Value (IdVal "fact");*)
    (*Value (LambdaVal [*)
        (*Dup; Value (IntVal 1); Gt; (* test for base case *)*)
        (*Value (LambdaVal [*)
            (*Dup; Value (IntVal 1); Minus; NamedFunction "fact"; Multiplication]); (* True (recursive) case *)*)
        (*Value (LambdaVal [*)
            (*Dup; Multiplication]); (* False (base) case *)*)
    (*If]);*)
    (*Define;*)
    (*]*)

(*let fact_acc: program = swap @ [*)
    (*Value (IdVal "fact_internal");*)
    (*Value (LambdaVal [*)
        (*Dup; Value (IntVal 1); Gt; (* test for base case *)*)
        (*Value (LambdaVal [ (* True (recursive) case *)*)
            (*Dup; *)
            (*Value (IntVal 2); Dollar; (* pull accumulator to top *)*)
            (*Multiplication; NamedFunction "swap";*)
            (*Value (IntVal 1); Minus;*)
            (*NamedFunction "fact_internal"]); *)
        (*Value (LambdaVal [ (* False (base) case *)*)
            (*Drop; (* remove counter; expose accumlator *)*)
            (*Value (IntVal 1); Multiplication]);*)
        (*If*)
    (*]);*)
    (*Define;*)
    (*Value (IdVal "fact");*)
    (*Value (LambdaVal [*)
        (*Value (IntVal 1); NamedFunction "swap"; (* setup accumulator *)*)
        (*NamedFunction "fact_internal"*)
    (*]);*)
    (*Define*)
    (*]*)

(* we should have a really simple parser for this *)
open Option
(* expects a list of strings and pulls out balanced pairs of opn, cls *)
(* NOTE: assumes the first ( is not present! I.e. ["Hi"; "there"; ")"]; returns Some (["Hi"; "there"], []) *)
let rec split_on_nest opn cls l =
    match l with
    | a :: l' -> 
        (* descend a scope *)
        if a = opn then 
            bind (split_on_nest opn cls l') (fun (inner, rest) ->
                bind (split_on_nest opn cls rest) (fun (outer, rest') -> Some (opn :: inner @ cls :: outer, rest')))
        (* end a scope *)
        else if a = cls then
            Some ([], l') 
        (* take an input *)
            else bind (split_on_nest opn cls l') (fun (i, o) -> Some (a :: i, o))
    | [] -> None


let add_to_arr (ArrowTy (i, o)) i_or_o add =
    if i_or_o then ArrowTy (add :: i, o) else ArrowTy (i, add :: o)

let rec parse_ty i_or_o l =
    match l with
    | "Int"  :: l' -> bind (parse_ty i_or_o l') (fun arrow -> Some (add_to_arr arrow i_or_o IntTy))
    | "Bool" :: l' -> bind (parse_ty i_or_o l') (fun arrow -> Some (add_to_arr arrow i_or_o BoolTy))
    | "Id"   :: l' -> bind (parse_ty i_or_o l') (fun arrow -> Some (add_to_arr arrow i_or_o IdTy))
    | "->"   :: l' -> bind (parse_ty false l') (fun arrow -> Some arrow)
    | "["    :: l' -> bind (grab_type_annot l') (fun (inner_arrow, rest) -> 
                        bind (parse_ty true rest) (fun arrow -> Some (add_to_arr arrow i_or_o inner_arrow)))
    | [] -> Some (ArrowTy ([], []))
    | _ -> None

and grab_type_annot lst =
    bind (split_on_nest "[" "]" lst) (fun (annot, lst') ->
    bind (parse_ty true annot) (fun arrow ->
    Some (arrow, lst')))


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
                bind (grab_type_annot wds) (fun (arrow, wds') ->
                    bind (split_on_nest "([" ")" wds') (fun (inside, outside) ->
                        bind (go inside) (fun prog ->
                            bind (go outside) (fun rest -> Some (Value (LambdaVal (prog, arrow)) :: rest)))))

        (* either a number or a named function *)
        | other :: wds -> bind (go wds) (fun rest -> 
                if Str.string_match num_reg other 0
                    then Some (Value (IntVal (int_of_string other)) :: rest)
                    else Some (NamedFunction other :: rest))

    in go (Str.split (Str.regexp "[ \t\n]+") s) 


(* checks if the program's current input stack has what the input stack from a 
   Lambda function needs and returns the new program input stack if success  *)
let rec match_stack lambdaStack inputStack : typ list option = 
    match lambdaStack, inputStack with 
    | [], _  -> Some inputStack
    | _, [] -> None
    | x :: lambdaS', y :: inputS' -> if x = y then match_stack lambdaS' inputS' else None


let rec typecheck (prog : program) (stack : typ list) (gamma : ty_context) =
    let rec typecheck_internal prog stack (gamma : ty_context) =
        match prog with
        | [] -> Some stack
        | Plus :: rest | Minus :: rest | Multiplication :: rest | Division :: rest | Modulo :: rest
            -> (match stack with
                | IntTy :: IntTy :: stack -> typecheck_internal rest (IntTy :: stack) gamma
                | _ -> None)

        | Eq :: rest | Lt :: rest | Gt :: rest | LtEq :: rest | GtEq :: rest | NotEq :: rest
            -> (match stack with
                | IntTy :: IntTy :: stack -> typecheck_internal rest (BoolTy :: stack) gamma
                | _ -> None)

        | And :: rest | Or :: rest
            -> (match stack with
                | BoolTy :: BoolTy :: stack -> typecheck_internal rest (BoolTy :: stack) gamma
                | _ -> None)

        | Not :: rest
            -> (match stack with
                | BoolTy :: stack -> typecheck_internal rest (BoolTy :: stack) gamma
                | _ -> None)

        | Dup :: rest
            -> (match stack with
                | a :: stack -> typecheck_internal rest (a :: a :: stack) gamma
                | _ -> None)

        | Drop :: rest | Dot :: rest
            -> (match stack with
                | a :: stack -> typecheck_internal rest stack gamma
                | _ -> None)

        | Value (IntVal n) :: Dollar :: rest
            -> Option.bind (pull_index n stack) (fun stack -> typecheck_internal rest stack gamma)
        | Dollar :: _ -> None (* prevent dependent type requirements *)

        | Value (IdVal id) :: Value (LambdaVal (impl, (ArrowTy (func_in, func_out) as l_type))) :: Define :: rest -> 
                let gamma' = IDMap.add id l_type gamma in 
                (match typecheck impl func_in gamma' with
                | Some ((ArrowTy (checked_in, checked_out)) as typ) -> 
                        if func_out = checked_out && checked_in = func_in then 
                        typecheck_internal rest (typ :: stack) gamma'
                else None
                | _ -> None)

        | Define :: _ -> None
        | NamedFunction word :: rest 
            -> (match IDMap.find_opt word gamma with 
                | Some (ArrowTy (inTyp, outTyp)) -> 
                                            (match match_stack inTyp stack with
                                             | Some result -> typecheck_internal rest (outTyp @ result) gamma
                                             | None -> None)
                | _ -> None)
            
        | Value (LambdaVal (impl, ArrowTy (func_in, func_out))) :: rest -> 
                (match typecheck impl func_in gamma with
                | Some ((ArrowTy (checked_in, checked_out)) as typ) -> if func_out = checked_out then 
                        typecheck_internal rest (typ :: stack) gamma
                else None
                | _ -> None)
        | If :: rest -> 
                (match stack with
                 | ArrowTy (i1, o1) :: ArrowTy (i2, o2) :: BoolTy :: stack -> 
                         if i1 = i2 && o1 = o2 then (
                             match match_stack i1 stack with
                             | Some result -> typecheck_internal rest (o1 @ result) gamma
                             | None -> None)
                         else None
                | _ -> None)


        (* base literals *)
        | Value (IntVal _) :: rest -> typecheck_internal rest (IntTy :: stack) gamma
        | Value (BoolVal _) :: rest -> typecheck_internal rest (BoolTy :: stack) gamma
        | Value (IdVal _) :: rest -> typecheck_internal rest (IdTy :: stack) gamma
        | _ -> None

    (* full typechecking is just mapping inputs to the output stack *)
    in let res = (match typecheck_internal prog stack gamma with
        | Some typ_list -> Some (ArrowTy (stack, typ_list))
        | None -> None) in res

(* so now we can write an interpreter! *)
let interpret ?(walk = false) (s : string) : config option =
    match parse s with
    | None -> print_string "Cannot parse program\n"; None
    | Some program -> 
            (match typecheck program [] IDMap.empty with
             | Some (ArrowTy ([], out_stack)) -> Some (if walk then walk_program program else run_program program)
             | _ -> print_string "Program does not typecheck\n"; None)

(* and proper programs! *)
let rec_test = "\\ a ([ Int -> Int ] dup 0 = ([ Int -> Int ] ) ([ Int -> Int ] 1 - a ) if ) define 5 a"

let fact_acc_str = "
\\ swap ([ Int Int -> Int Int ] 1 $ ) define 
\\ fact_internal ([ Int Int -> Int ] dup 1 > 
    ([ Int Int -> Int ] dup 2 $ * swap 1 - fact_internal ) 
    ([ Int Int -> Int ] drop ) if ) define 
\\ fact ([ Int -> Int ] 1 swap fact_internal ) define 5 fact"

(* calculates the next leapyear *)
let next_leapyear_str = "
\\ is_leapyear ([ Int -> Int Int ] dup 4 % ) define
\\ next_leapyear ([ Int -> Int ] is_leapyear 0 = ([ Int -> Int ] 4 + ) ([ Int -> Int ] is_leapyear 4 1 $ - + ) if ) define 
    2021 next_leapyear"


(* computes fibonachi numbers recursively *)
let fib_n_str = "
\\ swap ([ Int Int -> Int Int ] 1 $ ) define 
\\ fib ([ Int -> Int ] 
        dup 1 <=

        ([ Int -> Int ] drop 1 )
        ([ Int -> Int ] dup 1 - fib swap 2 - fib + )

        if
    ) define

\\ pfib ([ Int -> ] fib . ) define

0 pfib
1 pfib
2 pfib
3 pfib
4 pfib
5 pfib
6 pfib
7 pfib
8 pfib
9 pfib
10 pfib
11 pfib
12 pfib
13 pfib
14 pfib
15 pfib
16 pfib
17 pfib
18 pfib
19 pfib
20 pfib
"
