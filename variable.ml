(*from assignment 1*)
open Names

module VarStr : Names.VarSig with type variable = string = struct
  type variable = string

  let compare = String.compare
  let mk_var (s : string) = (s : variable)
  let tostr (s : variable) = (s : string)
end
