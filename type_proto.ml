
(* TODO: fix LambdaTyp *)
type typ = IdTyp | IntTyp | BoolTyp | LambdaTyp

(* takes in a value and returns the value type (or typ option?)
   the output will be whatever we push onto the stack for typechecking word *)
let rec type_of_value (v : value) : typ =
    match v with 
    | IdVal _ -> IdTyp
    | IntVal _ -> IntTyp
    | BoolVal _ -> BoolTyp
    | LambdaVal _ -> LambdaTyp (* typecheck word?  *)
    (* Error case? *)
  and typecheck (sigma, stack, p) : bool = 
  (* or 
  and typecheck (sigma, stack, p) : config *)
    match p, stack with 
    | [] -> true 

    | Value v :: p', _ -> typecheck p' sigma (type_of_value v :: stack) 

    | Plus :: p', v2 :: v1 :: s' | Minus :: p', v2 :: v1 :: s' | Multiplication :: p', v2 :: v1 :: s' 
    | Division :: p', v2 :: v1 :: s' | Modulo :: p', v2 :: v1 :: s' ->
      ( match type_of_value v2, type_of_value v1 with 
        | IntTyp, IntTyp -> typecheck p' sigma (IntTyp :: s') 
        | _, _ -> false )

    | Eq :: p', v2 :: v1 :: s' | NotEq :: p', v2 :: v1 :: s' | Lt :: p', v2 :: v1 :: s' 
    | LtEq:: p', v2 :: v1 :: s' | Gt :: p', v2 :: v1 :: s' | GtEq  :: p', v2 :: v1 :: s' -> 
      ( match type_of_value v2, type_of_value v1 with 
        | IntTyp, IntTyp -> typecheck p' sigma (BoolTyp :: s') 
        | _, _ -> false )

    | And :: p', v2 :: v1 :: s' | Or :: p', v2 :: v1 :: s' -> 
      ( match type_of_value v2, type_of_value v1 with 
        | BoolTyp, BoolTyp -> typecheck p' sigma (BoolTyp :: s') 
        | _, _ -> false )

    | Not :: p', v :: s' -> 
      ( match type_of_value v with 
        | BoolTyp -> typecheck p' sigma (BoolTyp :: s') 
        | _ -> false )

    | If :: p', v2 :: v1 :: v0 :: s' -> 
      ( match type_of_value v2, type_of_value v1, type_of_value v0 with 
        | LambdaTyp, LambdaTyp, BoolTyp -> typecheck p' sigma s' 
        | _, _ -> false )

    | Dup :: p', v :: s' -> 
        ( match type_of_value v with 
          | IdTyp -> typecheck p' sigma (IdTyp :: IdTyp :: s')
          | IntTyp -> typecheck p' sigma (IntTyp :: IntTyp :: s')
          | BoolTyp -> typecheck p' sigma (BoolTyp :: BoolTyp :: s')
          | LambdaTyp -> typecheck p' sigma (LambdaVal :: LambdaTyp :: s')
          | _ -> false)
    | Drop :: p', v :: s' -> 
        typecheck p' sigma s'
    
    (*TODO: | Dollar :: p', *)

    | Define :: p', v2 :: v1 :: s' -> 
      ( match type_of_value v2, type_of_value v1 with 
        | LambdaTyp, IdTyp -> typecheck p' sigma s' 
        | _, _ -> false )

    (*TODO: NamedFunction?
            Dot? *)
    | _, _ -> false (* if none of the above cases match, our program is not type correct*)

