open Declarations

type opcode =
  | LOOKUP of string
  | MKCLOS of string * opcode list
  | APP
  | RET
  | CONST of const
  | IF of opcode list * opcode list
  | MKPAIR
  | FST
  | SND
  | ADD
  | SUB
  | MUL
  | DIV
  | EQ
  | NOT
  | AND
  | OR

module FunTable = struct
  exception Missing_binding of string

  type answer =
    | Clo of clos
    | ConstV of const
    | PairV of answer * answer

  and table = string -> answer
  and clos = Clos of string * opcode list * table

  type stack = answer list
  type dump = (stack * table * opcode list) list

  exception Stuck of string

  let empty : table = fun x -> raise (Missing_binding x)
  let extend gamma x v : table = fun y -> if x = y then v else gamma y

  let remove gamma x : table =
   fun y -> if x = y then raise (Missing_binding y) else gamma y

  let lookup gamma x =
    try gamma x
    with Missing_binding _ ->
      raise (Stuck ("unbound variable in Secd.FunTable: " ^ x))

  (* compile translates a source expression into SECD code. *)
  let rec compile = function
    | V x -> [ LOOKUP x ]
    | App (e1, e2) -> compile e1 @ compile e2 @ [ APP ]
    | Abs (x, e1) -> [ MKCLOS (x, compile e1 @ [ RET ]) ]
    | Const c -> [ CONST c ]
    | Let (x, e1, e2) -> compile (App (Abs (x, e2), e1))
    | IfTE (c, t, f) -> compile c @ [ IF (compile t, compile f) ]
    | Pair (e1, e2) -> compile e1 @ compile e2 @ [ MKPAIR ]
    | Fst e -> compile e @ [ FST ]
    | Snd e -> compile e @ [ SND ]
    | Add (e1, e2) -> compile e1 @ compile e2 @ [ ADD ]
    | Sub (e1, e2) -> compile e1 @ compile e2 @ [ SUB ]
    | Mul (e1, e2) -> compile e1 @ compile e2 @ [ MUL ]
    | Div (e1, e2) -> compile e1 @ compile e2 @ [ DIV ]
    | Eq (e1, e2) -> compile e1 @ compile e2 @ [ EQ ]
    | Not e -> compile e @ [ NOT ]
    | And (e1, e2) -> compile e1 @ compile e2 @ [ AND ]
    | Or (e1, e2) -> compile e1 @ compile e2 @ [ OR ]

  let rec decompile code =
    let rec go stack = function
      | [] -> (
          match stack with
          | [ e ] -> e
          | _ -> raise (Stuck "malformed opcode sequence during decompile"))
      | LOOKUP x :: rest -> go (V x :: stack) rest
      | MKCLOS (x, c) :: rest -> go (Abs (x, decompile_body c) :: stack) rest
      | APP :: rest -> (
          match stack with
          | e2 :: e1 :: st -> go (App (e1, e2) :: st) rest
          | _ -> raise (Stuck "APP expects two expressions during decompile"))
      | RET :: _ ->
          raise (Stuck "RET should only appear at the end of function code")
      | CONST c :: rest -> go (Const c :: stack) rest
      | IF (ct, cf) :: rest -> (
          match stack with
          | c :: st -> go (IfTE (c, decompile ct, decompile cf) :: st) rest
          | _ -> raise (Stuck "IF expects one expression during decompile"))
      | MKPAIR :: rest -> (
          match stack with
          | e2 :: e1 :: st -> go (Pair (e1, e2) :: st) rest
          | _ -> raise (Stuck "MKPAIR expects two expressions during decompile"))
      | FST :: rest -> (
          match stack with
          | e :: st -> go (Fst e :: st) rest
          | _ -> raise (Stuck "FST expects one expression during decompile"))
      | SND :: rest -> (
          match stack with
          | e :: st -> go (Snd e :: st) rest
          | _ -> raise (Stuck "SND expects one expression during decompile"))
      | ADD :: rest -> decompile_binary (fun e1 e2 -> Add (e1, e2)) stack rest "ADD"
      | SUB :: rest -> decompile_binary (fun e1 e2 -> Sub (e1, e2)) stack rest "SUB"
      | MUL :: rest -> decompile_binary (fun e1 e2 -> Mul (e1, e2)) stack rest "MUL"
      | DIV :: rest -> decompile_binary (fun e1 e2 -> Div (e1, e2)) stack rest "DIV"
      | EQ :: rest -> decompile_binary (fun e1 e2 -> Eq (e1, e2)) stack rest "EQ"
      | NOT :: rest -> (
          match stack with
          | e :: st -> go (Not e :: st) rest
          | _ -> raise (Stuck "NOT expects one expression during decompile"))
      | AND :: rest -> decompile_binary (fun e1 e2 -> And (e1, e2)) stack rest "AND"
      | OR :: rest -> decompile_binary (fun e1 e2 -> Or (e1, e2)) stack rest "OR"
    and decompile_binary mk stack rest name =
      match stack with
      | e2 :: e1 :: st -> go (mk e1 e2 :: st) rest
      | _ -> raise (Stuck (name ^ " expects two expressions during decompile"))
    and decompile_body c =
      match List.rev c with
      | RET :: rev_body -> go [] (List.rev rev_body)
      | _ -> raise (Stuck "function code must end in RET")
    in
    go [] code

  let decompile_closure_body = function
    | [] -> raise (Stuck "empty function body in Secd.FunTable")
    | c -> (
        match List.rev c with
        | RET :: rev_body -> decompile (List.rev rev_body)
        | _ -> raise (Stuck "function code must end in RET"))

  (* unpack_exp rebuilds an expression by resolving bindings stored in an environment. *)
  let rec unpack_exp gamma = function
    | V x -> ( try unpack_answer (gamma x) with Missing_binding _ -> V x)
    | Abs (x, e1) -> Abs (x, unpack_exp (remove gamma x) e1)
    | App (e1, e2) -> App (unpack_exp gamma e1, unpack_exp gamma e2)
    | Const c -> Const c
    | Let (x, e1, e2) -> Let (x, unpack_exp gamma e1, unpack_exp (remove gamma x) e2)
    | IfTE (c, t, f) -> IfTE (unpack_exp gamma c, unpack_exp gamma t, unpack_exp gamma f)
    | Pair (e1, e2) -> Pair (unpack_exp gamma e1, unpack_exp gamma e2)
    | Fst e -> Fst (unpack_exp gamma e)
    | Snd e -> Snd (unpack_exp gamma e)
    | Add (e1, e2) -> Add (unpack_exp gamma e1, unpack_exp gamma e2)
    | Sub (e1, e2) -> Sub (unpack_exp gamma e1, unpack_exp gamma e2)
    | Mul (e1, e2) -> Mul (unpack_exp gamma e1, unpack_exp gamma e2)
    | Div (e1, e2) -> Div (unpack_exp gamma e1, unpack_exp gamma e2)
    | Eq (e1, e2) -> Eq (unpack_exp gamma e1, unpack_exp gamma e2)
    | Not e -> Not (unpack_exp gamma e)
    | And (e1, e2) -> And (unpack_exp gamma e1, unpack_exp gamma e2)
    | Or (e1, e2) -> Or (unpack_exp gamma e1, unpack_exp gamma e2)

  and unpack_answer = function
    | Clo (Clos (x, c, gamma)) ->
        Abs (x, unpack_exp (remove gamma x) (decompile_closure_body c))
    | ConstV c -> Const c
    | PairV (v1, v2) -> Pair (unpack_answer v1, unpack_answer v2)

  let is_final = function [ _ ], _, [], [] -> true | _ -> false

  let step = function
    | s, gamma, LOOKUP x :: c', d -> (lookup gamma x :: s, gamma, c', d)
    | s, gamma, MKCLOS (x, c1) :: c', d ->
        (Clo (Clos (x, c1, gamma)) :: s, gamma, c', d)
    | s, gamma, CONST c :: c', d -> (ConstV c :: s, gamma, c', d)
    | ConstV (CBool b) :: s, gamma, IF (ct, cf) :: c', d ->
        (s, gamma, (if b then ct else cf) @ c', d)
    | arg :: Clo (Clos (x, c1, gamma1)) :: s, gamma, APP :: c', d ->
        ([], extend gamma1 x arg, c1, (s, gamma, c') :: d)
    | v2 :: v1 :: s, gamma, MKPAIR :: c', d -> (PairV (v1, v2) :: s, gamma, c', d)
    | PairV (v1, _) :: s, gamma, FST :: c', d -> (v1 :: s, gamma, c', d)
    | PairV (_, v2) :: s, gamma, SND :: c', d -> (v2 :: s, gamma, c', d)
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, ADD :: c', d ->
        (ConstV (CInt (n1 + n2)) :: s, gamma, c', d)
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, SUB :: c', d ->
        (ConstV (CInt (n1 - n2)) :: s, gamma, c', d)
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, MUL :: c', d ->
        (ConstV (CInt (n1 * n2)) :: s, gamma, c', d)
    | ConstV (CInt 0) :: ConstV (CInt _) :: _, _, DIV :: _, _ ->
        raise (Stuck "division by zero in Secd.FunTable")
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, DIV :: c', d ->
        (ConstV (CInt (n1 / n2)) :: s, gamma, c', d)
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, EQ :: c', d ->
        (ConstV (CBool (n1 = n2)) :: s, gamma, c', d)
    | ConstV (CBool b) :: s, gamma, NOT :: c', d ->
        (ConstV (CBool (not b)) :: s, gamma, c', d)
    | ConstV (CBool b2) :: ConstV (CBool b1) :: s, gamma, AND :: c', d ->
        (ConstV (CBool (b1 && b2)) :: s, gamma, c', d)
    | ConstV (CBool b2) :: ConstV (CBool b1) :: s, gamma, OR :: c', d ->
        (ConstV (CBool (b1 || b2)) :: s, gamma, c', d)
    | v :: _, _, RET :: _, (s, gamma, c') :: d -> (v :: s, gamma, c', d)
    | _, _, [], [] ->
        raise (Stuck "machine is already in a final state in Secd.FunTable")
    | _ -> raise (Stuck "ill-formed SECD configuration in Secd.FunTable")

  let rec secd conf = if is_final conf then conf else secd (step conf)

  (* cbv runs the SECD machine and returns the unpacked result. *)
  let cbv e =
    let s, _, _, _ = secd ([], empty, compile e, []) in
    match s with
    | v :: _ -> unpack_answer v
    | [] -> raise (Stuck "empty stack after SECD execution in Secd.FunTable")
end

module ListTable = struct
  type answer =
    | Clo of clos
    | ConstV of const
    | PairV of answer * answer

  and table = (string * answer) list
  and clos = Clos of string * opcode list * table

  type stack = answer list
  type dump = (stack * table * opcode list) list

  exception Stuck of string
  exception Missing_binding of string

  let empty : table = []
  let extend gamma x v = (x, v) :: gamma
  let remove gamma x = List.filter (fun (y, _) -> x <> y) gamma

  let rec lookup_raw gamma x =
    match gamma with
    | [] -> raise (Missing_binding x)
    | (y, v) :: rest -> if x = y then v else lookup_raw rest x

  let lookup gamma x =
    try lookup_raw gamma x
    with Missing_binding _ ->
      raise (Stuck ("unbound variable in Secd.ListTable: " ^ x))

  (* compile translates a source expression into SECD code. *)
  let compile = FunTable.compile
  let decompile = FunTable.decompile
  let decompile_closure_body = FunTable.decompile_closure_body

  (* unpack_exp rebuilds an expression by resolving bindings stored in an environment. *)
  let rec unpack_exp gamma = function
    | V x -> (
        try unpack_answer (lookup_raw gamma x) with Missing_binding _ -> V x)
    | Abs (x, e1) -> Abs (x, unpack_exp (remove gamma x) e1)
    | App (e1, e2) -> App (unpack_exp gamma e1, unpack_exp gamma e2)
    | Const c -> Const c
    | Let (x, e1, e2) -> Let (x, unpack_exp gamma e1, unpack_exp (remove gamma x) e2)
    | IfTE (c, t, f) -> IfTE (unpack_exp gamma c, unpack_exp gamma t, unpack_exp gamma f)
    | Pair (e1, e2) -> Pair (unpack_exp gamma e1, unpack_exp gamma e2)
    | Fst e -> Fst (unpack_exp gamma e)
    | Snd e -> Snd (unpack_exp gamma e)
    | Add (e1, e2) -> Add (unpack_exp gamma e1, unpack_exp gamma e2)
    | Sub (e1, e2) -> Sub (unpack_exp gamma e1, unpack_exp gamma e2)
    | Mul (e1, e2) -> Mul (unpack_exp gamma e1, unpack_exp gamma e2)
    | Div (e1, e2) -> Div (unpack_exp gamma e1, unpack_exp gamma e2)
    | Eq (e1, e2) -> Eq (unpack_exp gamma e1, unpack_exp gamma e2)
    | Not e -> Not (unpack_exp gamma e)
    | And (e1, e2) -> And (unpack_exp gamma e1, unpack_exp gamma e2)
    | Or (e1, e2) -> Or (unpack_exp gamma e1, unpack_exp gamma e2)

  and unpack_answer = function
    | Clo (Clos (x, c, gamma)) ->
        Abs (x, unpack_exp (remove gamma x) (decompile_closure_body c))
    | ConstV c -> Const c
    | PairV (v1, v2) -> Pair (unpack_answer v1, unpack_answer v2)

  let is_final = function [ _ ], _, [], [] -> true | _ -> false

  let step = function
    | s, gamma, LOOKUP x :: c', d -> (lookup gamma x :: s, gamma, c', d)
    | s, gamma, MKCLOS (x, c1) :: c', d ->
        (Clo (Clos (x, c1, gamma)) :: s, gamma, c', d)
    | s, gamma, CONST c :: c', d -> (ConstV c :: s, gamma, c', d)
    | ConstV (CBool b) :: s, gamma, IF (ct, cf) :: c', d ->
        (s, gamma, (if b then ct else cf) @ c', d)
    | arg :: Clo (Clos (x, c1, gamma1)) :: s, gamma, APP :: c', d ->
        ([], extend gamma1 x arg, c1, (s, gamma, c') :: d)
    | v2 :: v1 :: s, gamma, MKPAIR :: c', d -> (PairV (v1, v2) :: s, gamma, c', d)
    | PairV (v1, _) :: s, gamma, FST :: c', d -> (v1 :: s, gamma, c', d)
    | PairV (_, v2) :: s, gamma, SND :: c', d -> (v2 :: s, gamma, c', d)
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, ADD :: c', d ->
        (ConstV (CInt (n1 + n2)) :: s, gamma, c', d)
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, SUB :: c', d ->
        (ConstV (CInt (n1 - n2)) :: s, gamma, c', d)
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, MUL :: c', d ->
        (ConstV (CInt (n1 * n2)) :: s, gamma, c', d)
    | ConstV (CInt 0) :: ConstV (CInt _) :: _, _, DIV :: _, _ ->
        raise (Stuck "division by zero in Secd.ListTable")
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, DIV :: c', d ->
        (ConstV (CInt (n1 / n2)) :: s, gamma, c', d)
    | ConstV (CInt n2) :: ConstV (CInt n1) :: s, gamma, EQ :: c', d ->
        (ConstV (CBool (n1 = n2)) :: s, gamma, c', d)
    | ConstV (CBool b) :: s, gamma, NOT :: c', d ->
        (ConstV (CBool (not b)) :: s, gamma, c', d)
    | ConstV (CBool b2) :: ConstV (CBool b1) :: s, gamma, AND :: c', d ->
        (ConstV (CBool (b1 && b2)) :: s, gamma, c', d)
    | ConstV (CBool b2) :: ConstV (CBool b1) :: s, gamma, OR :: c', d ->
        (ConstV (CBool (b1 || b2)) :: s, gamma, c', d)
    | v :: _, _, RET :: _, (s, gamma, c') :: d -> (v :: s, gamma, c', d)
    | _, _, [], [] ->
        raise (Stuck "machine is already in a final state in Secd.ListTable")
    | _ -> raise (Stuck "ill-formed SECD configuration in Secd.ListTable")

  let rec secd conf = if is_final conf then conf else secd (step conf)

  (* cbv runs the SECD machine and returns the unpacked result. *)
  let cbv e =
    let s, _, _, _ = secd ([], empty, compile e, []) in
    match s with
    | v :: _ -> unpack_answer v
    | [] -> raise (Stuck "empty stack after SECD execution in Secd.ListTable")
end
