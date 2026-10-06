open Symbol
open Variable
open Bigint

type atom =
  | Num of Bigint.BigNum.bigint
  | Ident of Variable.VarStr.variable
  | Sym of Symbol.SymbolStr.symbol

type comment = Comment of string
type exp = A of atom | L of exp list
type program = Program of exp list
type typ =
  | Tint
  | Tbool
  | Tvar
  | Tlist of int
  | TlistAny
  | Tfunc of int * typ
  | TfuncMin of int * typ
type type_env = (string * typ list) list
