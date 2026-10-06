type variable = string
type const = CInt of int | CBool of bool

type exp =
  | V of variable
  | Abs of variable * exp
  | App of exp * exp
  | Const of const
  | Let of variable * exp * exp
  | IfTE of exp * exp * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
  | Eq of exp * exp
  | Not of exp
  | And of exp * exp
  | Or of exp * exp

let var x = V x
let lam x e = Abs (x, e)
let app e1 e2 = App (e1, e2)
let int n = Const (CInt n)
let bool b = Const (CBool b)
let let_ x e1 e2 = Let (x, e1, e2)
let if_ c t f = app (app c t) f
let ifte c t f = IfTE (c, t, f)
let pair_ e1 e2 = Pair (e1, e2)
let fst_ e = Fst e
let snd_ e = Snd e
let add e1 e2 = Add (e1, e2)
let sub e1 e2 = Sub (e1, e2)
let mul e1 e2 = Mul (e1, e2)
let div e1 e2 = Div (e1, e2)
let eq e1 e2 = Eq (e1, e2)
let not_ e = Not e
let and_ e1 e2 = And (e1, e2)
let or_ e1 e2 = Or (e1, e2)

(* string_of_exp pretty-prints expressions for test output. *)
let rec string_of_exp = function
  | V x -> x
  | Abs (x, e) -> "(\\" ^ x ^ "." ^ string_of_exp e ^ ")"
  | App (e1, e2) -> "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | Const (CInt n) -> string_of_int n
  | Const (CBool true) -> "true"
  | Const (CBool false) -> "false"
  | Let (x, e1, e2) ->
      "(let " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
  | IfTE (c, t, f) ->
      "(if " ^ string_of_exp c ^ " then " ^ string_of_exp t ^ " else "
      ^ string_of_exp f ^ ")"
  | Pair (e1, e2) -> "(pair " ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Fst e -> "(fst " ^ string_of_exp e ^ ")"
  | Snd e -> "(snd " ^ string_of_exp e ^ ")"
  | Add (e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ string_of_exp e1 ^ " - " ^ string_of_exp e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
  | Div (e1, e2) -> "(" ^ string_of_exp e1 ^ " / " ^ string_of_exp e2 ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_exp e1 ^ " = " ^ string_of_exp e2 ^ ")"
  | Not e -> "(not " ^ string_of_exp e ^ ")"
  | And (e1, e2) -> "(" ^ string_of_exp e1 ^ " && " ^ string_of_exp e2 ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_exp e1 ^ " || " ^ string_of_exp e2 ^ ")"

let tru = lam "t" (lam "f" (var "t"))
let fls = lam "t" (lam "f" (var "f"))
let if_true_int = if_ tru (int 7) (int 9)
let if_false_int = if_ fls (int 7) (int 9)
