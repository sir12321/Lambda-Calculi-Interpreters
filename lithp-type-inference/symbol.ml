(*from assignment 1*)
open Names

module SymbolStr : Names.SymbolSig with type name_type = string = struct
  type name_type = string
  type symbol = { name : name_type; arity : int }

  let mk_sym n a = { name = n; arity = a }
  let tostr s = s.name
  let arity s = s.arity
  let compare s1 s2 = String.compare s1.name s2.name
end
