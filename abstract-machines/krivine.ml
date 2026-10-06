open Declarations

module FunTable = struct
  exception Missing_binding of string

  type table = string -> clos
  and clos = Clos of exp * table

  type frame =
    | Arg of clos
    | IfFrame of clos * clos
    | FstProj
    | SndProj
    | NotFrame
    | AddLeft of clos
    | AddRight of int
    | SubLeft of clos
    | SubRight of int
    | MulLeft of clos
    | MulRight of int
    | DivLeft of clos
    | DivRight of int
    | EqLeft of clos
    | EqRight of int
    | AndLeft of clos
    | OrLeft of clos

  type stack = frame list

  exception Stuck of string

  let empty : table = fun x -> raise (Missing_binding x)

  let extend (gamma : table) x cl : table =
   fun y -> if x = y then cl else gamma y

  let remove (gamma : table) x : table =
   fun y -> if x = y then raise (Missing_binding y) else gamma y

  let lookup (gamma : table) x =
    try gamma x
    with Missing_binding _ ->
      raise (Stuck ("unbound variable in Krivine.FunTable: " ^ x))

  (* unpack converts a final closure back into a source-level expression. *)
  let rec unpack (Clos (e, gamma)) =
    match e with
    | V x -> ( try unpack (gamma x) with Missing_binding _ -> V x)
    | Abs (x, e1) -> Abs (x, unpack (Clos (e1, remove gamma x)))
    | App (e1, e2) -> App (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Const c -> Const c
    | Let (x, e1, e2) ->
        Let (x, unpack (Clos (e1, gamma)), unpack (Clos (e2, remove gamma x)))
    | IfTE (c, t, f) ->
        IfTE (unpack (Clos (c, gamma)), unpack (Clos (t, gamma)), unpack (Clos (f, gamma)))
    | Pair (e1, e2) ->
        Pair (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Fst e1 -> Fst (unpack (Clos (e1, gamma)))
    | Snd e1 -> Snd (unpack (Clos (e1, gamma)))
    | Add (e1, e2) -> Add (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Sub (e1, e2) -> Sub (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Mul (e1, e2) -> Mul (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Div (e1, e2) -> Div (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Eq (e1, e2) -> Eq (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Not e1 -> Not (unpack (Clos (e1, gamma)))
    | And (e1, e2) -> And (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Or (e1, e2) -> Or (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))

  let is_final = function
    | Clos (Abs _, _), []
    | Clos (Const (CInt _), _), []
    | Clos (Const (CBool _), _), []
    | Clos (Pair _, _), [] ->
        true
    | _ -> false

  let step = function
    | Clos (App (e1, e2), gamma), s ->
        (Clos (e1, gamma), Arg (Clos (e2, gamma)) :: s)
    | Clos (V x, gamma), s -> (lookup gamma x, s)
    | Clos (Abs (x, e1), gamma), Arg cl :: s -> (Clos (e1, extend gamma x cl), s)
    | Clos (Let (x, e1, e2), gamma), s -> (Clos (e2, extend gamma x (Clos (e1, gamma))), s)
    | Clos (IfTE (c, t, f), gamma), s ->
        (Clos (c, gamma), IfFrame (Clos (t, gamma), Clos (f, gamma)) :: s)
    | Clos (Const (CBool true), _), IfFrame (cl_t, _) :: s -> (cl_t, s)
    | Clos (Const (CBool false), _), IfFrame (_, cl_f) :: s -> (cl_f, s)
    | Clos (Pair (e1, _), gamma), FstProj :: s -> (Clos (e1, gamma), s)
    | Clos (Pair (_, e2), gamma), SndProj :: s -> (Clos (e2, gamma), s)
    | Clos (Fst e1, gamma), s -> (Clos (e1, gamma), FstProj :: s)
    | Clos (Snd e1, gamma), s -> (Clos (e1, gamma), SndProj :: s)
    | Clos (Not e1, gamma), s -> (Clos (e1, gamma), NotFrame :: s)
    | Clos (Const (CBool b), gamma), NotFrame :: s ->
        (Clos (Const (CBool (not b)), gamma), s)
    | Clos (Add (e1, e2), gamma), s ->
        (Clos (e1, gamma), AddLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), AddLeft cl2 :: s -> (cl2, AddRight n :: s)
    | Clos (Const (CInt m), gamma), AddRight n :: s ->
        (Clos (Const (CInt (n + m)), gamma), s)
    | Clos (Sub (e1, e2), gamma), s ->
        (Clos (e1, gamma), SubLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), SubLeft cl2 :: s -> (cl2, SubRight n :: s)
    | Clos (Const (CInt m), gamma), SubRight n :: s ->
        (Clos (Const (CInt (n - m)), gamma), s)
    | Clos (Mul (e1, e2), gamma), s ->
        (Clos (e1, gamma), MulLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), MulLeft cl2 :: s -> (cl2, MulRight n :: s)
    | Clos (Const (CInt m), gamma), MulRight n :: s ->
        (Clos (Const (CInt (n * m)), gamma), s)
    | Clos (Div (e1, e2), gamma), s ->
        (Clos (e1, gamma), DivLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), DivLeft cl2 :: s -> (cl2, DivRight n :: s)
    | Clos (Const (CInt 0), _), DivRight _ :: _ ->
        raise (Stuck "division by zero in Krivine.FunTable")
    | Clos (Const (CInt m), gamma), DivRight n :: s ->
        (Clos (Const (CInt (n / m)), gamma), s)
    | Clos (Eq (e1, e2), gamma), s ->
        (Clos (e1, gamma), EqLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), EqLeft cl2 :: s -> (cl2, EqRight n :: s)
    | Clos (Const (CInt m), gamma), EqRight n :: s ->
        (Clos (Const (CBool (n = m)), gamma), s)
    | Clos (And (e1, e2), gamma), s ->
        (Clos (e1, gamma), AndLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CBool false), gamma), AndLeft _ :: s ->
        (Clos (Const (CBool false), gamma), s)
    | Clos (Const (CBool true), _), AndLeft cl2 :: s -> (cl2, s)
    | Clos (Or (e1, e2), gamma), s ->
        (Clos (e1, gamma), OrLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CBool true), gamma), OrLeft _ :: s ->
        (Clos (Const (CBool true), gamma), s)
    | Clos (Const (CBool false), _), OrLeft cl2 :: s -> (cl2, s)
    | Clos (Abs _, _), []
    | Clos (Const (CInt _), _), []
    | Clos (Const (CBool _), _), []
    | Clos (Pair _, _), [] ->
        raise (Stuck "machine is already in a final state in Krivine.FunTable")
    | _ -> raise (Stuck "ill-formed configuration in Krivine.FunTable")

  let rec krivine conf = if is_final conf then conf else krivine (step conf)

  (* cbn runs the Krivine machine and returns the unpacked result. *)
  let cbn e =
    let cl, _ = krivine (Clos (e, empty), []) in
    unpack cl
end

module ListTable = struct
  type table = (string * clos) list
  and clos = Clos of exp * table

  type frame =
    | Arg of clos
    | IfFrame of clos * clos
    | FstProj
    | SndProj
    | NotFrame
    | AddLeft of clos
    | AddRight of int
    | SubLeft of clos
    | SubRight of int
    | MulLeft of clos
    | MulRight of int
    | DivLeft of clos
    | DivRight of int
    | EqLeft of clos
    | EqRight of int
    | AndLeft of clos
    | OrLeft of clos

  type stack = frame list

  exception Stuck of string
  exception Missing_binding of string

  let empty : table = []
  let extend gamma x cl = (x, cl) :: gamma
  let remove gamma x = List.filter (fun (y, _) -> x <> y) gamma

  let rec lookup_raw gamma x =
    match gamma with
    | [] -> raise (Missing_binding x)
    | (y, cl) :: rest -> if x = y then cl else lookup_raw rest x

  let lookup gamma x =
    try lookup_raw gamma x
    with Missing_binding _ ->
      raise (Stuck ("unbound variable in Krivine.ListTable: " ^ x))

  (* unpack converts a final closure back into a source-level expression. *)
  let rec unpack (Clos (e, gamma)) =
    match e with
    | V x -> ( try unpack (lookup_raw gamma x) with Missing_binding _ -> V x)
    | Abs (x, e1) -> Abs (x, unpack (Clos (e1, remove gamma x)))
    | App (e1, e2) -> App (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Const c -> Const c
    | Let (x, e1, e2) ->
        Let (x, unpack (Clos (e1, gamma)), unpack (Clos (e2, remove gamma x)))
    | IfTE (c, t, f) ->
        IfTE (unpack (Clos (c, gamma)), unpack (Clos (t, gamma)), unpack (Clos (f, gamma)))
    | Pair (e1, e2) ->
        Pair (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Fst e1 -> Fst (unpack (Clos (e1, gamma)))
    | Snd e1 -> Snd (unpack (Clos (e1, gamma)))
    | Add (e1, e2) -> Add (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Sub (e1, e2) -> Sub (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Mul (e1, e2) -> Mul (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Div (e1, e2) -> Div (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Eq (e1, e2) -> Eq (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Not e1 -> Not (unpack (Clos (e1, gamma)))
    | And (e1, e2) -> And (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
    | Or (e1, e2) -> Or (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))

  let is_final = function
    | Clos (Abs _, _), []
    | Clos (Const (CInt _), _), []
    | Clos (Const (CBool _), _), []
    | Clos (Pair _, _), [] ->
        true
    | _ -> false

  let step = function
    | Clos (App (e1, e2), gamma), s ->
        (Clos (e1, gamma), Arg (Clos (e2, gamma)) :: s)
    | Clos (V x, gamma), s -> (lookup gamma x, s)
    | Clos (Abs (x, e1), gamma), Arg cl :: s -> (Clos (e1, extend gamma x cl), s)
    | Clos (Let (x, e1, e2), gamma), s -> (Clos (e2, extend gamma x (Clos (e1, gamma))), s)
    | Clos (IfTE (c, t, f), gamma), s ->
        (Clos (c, gamma), IfFrame (Clos (t, gamma), Clos (f, gamma)) :: s)
    | Clos (Const (CBool true), _), IfFrame (cl_t, _) :: s -> (cl_t, s)
    | Clos (Const (CBool false), _), IfFrame (_, cl_f) :: s -> (cl_f, s)
    | Clos (Pair (e1, _), gamma), FstProj :: s -> (Clos (e1, gamma), s)
    | Clos (Pair (_, e2), gamma), SndProj :: s -> (Clos (e2, gamma), s)
    | Clos (Fst e1, gamma), s -> (Clos (e1, gamma), FstProj :: s)
    | Clos (Snd e1, gamma), s -> (Clos (e1, gamma), SndProj :: s)
    | Clos (Not e1, gamma), s -> (Clos (e1, gamma), NotFrame :: s)
    | Clos (Const (CBool b), gamma), NotFrame :: s ->
        (Clos (Const (CBool (not b)), gamma), s)
    | Clos (Add (e1, e2), gamma), s ->
        (Clos (e1, gamma), AddLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), AddLeft cl2 :: s -> (cl2, AddRight n :: s)
    | Clos (Const (CInt m), gamma), AddRight n :: s ->
        (Clos (Const (CInt (n + m)), gamma), s)
    | Clos (Sub (e1, e2), gamma), s ->
        (Clos (e1, gamma), SubLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), SubLeft cl2 :: s -> (cl2, SubRight n :: s)
    | Clos (Const (CInt m), gamma), SubRight n :: s ->
        (Clos (Const (CInt (n - m)), gamma), s)
    | Clos (Mul (e1, e2), gamma), s ->
        (Clos (e1, gamma), MulLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), MulLeft cl2 :: s -> (cl2, MulRight n :: s)
    | Clos (Const (CInt m), gamma), MulRight n :: s ->
        (Clos (Const (CInt (n * m)), gamma), s)
    | Clos (Div (e1, e2), gamma), s ->
        (Clos (e1, gamma), DivLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), DivLeft cl2 :: s -> (cl2, DivRight n :: s)
    | Clos (Const (CInt 0), _), DivRight _ :: _ ->
        raise (Stuck "division by zero in Krivine.ListTable")
    | Clos (Const (CInt m), gamma), DivRight n :: s ->
        (Clos (Const (CInt (n / m)), gamma), s)
    | Clos (Eq (e1, e2), gamma), s ->
        (Clos (e1, gamma), EqLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CInt n), _), EqLeft cl2 :: s -> (cl2, EqRight n :: s)
    | Clos (Const (CInt m), gamma), EqRight n :: s ->
        (Clos (Const (CBool (n = m)), gamma), s)
    | Clos (And (e1, e2), gamma), s ->
        (Clos (e1, gamma), AndLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CBool false), gamma), AndLeft _ :: s ->
        (Clos (Const (CBool false), gamma), s)
    | Clos (Const (CBool true), _), AndLeft cl2 :: s -> (cl2, s)
    | Clos (Or (e1, e2), gamma), s ->
        (Clos (e1, gamma), OrLeft (Clos (e2, gamma)) :: s)
    | Clos (Const (CBool true), gamma), OrLeft _ :: s ->
        (Clos (Const (CBool true), gamma), s)
    | Clos (Const (CBool false), _), OrLeft cl2 :: s -> (cl2, s)
    | Clos (Abs _, _), []
    | Clos (Const (CInt _), _), []
    | Clos (Const (CBool _), _), []
    | Clos (Pair _, _), [] ->
        raise (Stuck "machine is already in a final state in Krivine.ListTable")
    | _ -> raise (Stuck "ill-formed configuration in Krivine.ListTable")

  let rec krivine conf = if is_final conf then conf else krivine (step conf)

  (* cbn runs the Krivine machine and returns the unpacked result. *)
  let cbn e =
    let cl, _ = krivine (Clos (e, empty), []) in
    unpack cl
end
