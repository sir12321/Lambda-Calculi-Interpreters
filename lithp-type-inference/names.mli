module type SymbolSig = sig
  type name_type
  type symbol

  val mk_sym : name_type -> int -> symbol
  val arity : symbol -> int
  val compare : symbol -> symbol -> int
  val tostr : symbol -> string
end

module type VarSig = sig
  type variable

  val compare : variable -> variable -> int
  val mk_var : string -> variable
  val tostr : variable -> string
end

module type BignumSig = sig
  type sign
  type bigint
  type myBool

  exception Div_by_zero
  exception Not_an_integer

  val absolute : bigint -> bigint
  val unary_negation : bigint -> bigint
  val equal : bigint -> bigint -> myBool
  val greater_than : bigint -> bigint -> myBool
  val less_than : bigint -> bigint -> myBool
  val great_or_equal : bigint -> bigint -> myBool
  val less_or_equal : bigint -> bigint -> myBool
  val pretty_print : bigint -> string
  val int_to_bigint : int -> bigint
  val string_to_bigint : string -> bigint
  val addition : bigint -> bigint -> bigint
  val subtraction : bigint -> bigint -> bigint
  val multiplication : bigint -> bigint -> bigint
  val quotient : bigint -> bigint -> bigint
  val remainder : bigint -> bigint -> bigint
  val myBool2Bool : myBool -> bool
end
