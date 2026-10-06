open Ast

let name_of_exp (e : exp) : string =
  match e with
  | A (Sym s) -> Symbol.SymbolStr.tostr s
  | A (Ident id) -> Variable.VarStr.tostr id
  | _ -> ""

let is_cxr_name (name : string) : bool =
  let len = String.length name in
  len >= 4
  && name.[0] = 'c'
  && name.[len - 1] = 'r'
  && String.fold_left
       (fun acc c -> acc && (c = 'a' || c = 'd'))
       true
       (String.sub name 1 (len - 2))

let is_primitive_name (name : string) : bool =
  match name with
  | "+" | "*" | "-" | "div" | "mod" | ">" | "<" | "<=" | ">=" | "not"
  | "and" | "or" | "'" | "quote" | "atom" | "eq" | "car" | "cdr" | "cons"
  | "cond" | "lambda" | "label" | "defun" ->
      true
  | _ -> is_cxr_name name

let expand_cxr (name : string) (arg : exp) : exp =
  let rec build i acc =
    if i = 0 then acc
    else
      let op = if name.[i] = 'a' then "car" else "cdr" in
      let op_sym = A (Sym (Symbol.SymbolStr.mk_sym op 1)) in
      build (i - 1) (L [ op_sym; acc ])
  in
  build (String.length name - 2) arg

let add_type_if_missing (ts : typ list) (t : typ) : typ list =
  if List.mem t ts then ts else ts @ [ t ]

let union_types (a : typ list) (b : typ list) : typ list =
  List.fold_left add_type_if_missing a b

let list_lengths (ts : typ list) : int list =
  List.fold_left
    (fun acc t ->
      match t with
      | Tlist n -> if List.mem n acc then acc else n :: acc
      | _ -> acc)
    [] ts

let rec compatible_type (a : typ) (b : typ) : bool =
  match (a, b) with
  | Tvar, Tvar -> true
  | TlistAny, TlistAny -> true
  | TlistAny, Tlist _ | Tlist _, TlistAny -> true
  | Tlist n, Tlist m when n = m -> true
  | Tfunc (n1, ret1), Tfunc (n2, ret2) -> n1 = n2 && compatible_type ret1 ret2
  | Tfunc (n, ret1), TfuncMin (m, ret2) | TfuncMin (m, ret2), Tfunc (n, ret1) ->
      n >= m && compatible_type ret1 ret2
  | TfuncMin (n1, ret1), TfuncMin (n2, ret2) ->
      compatible_type ret1 ret2 && (n1 >= n2 || n2 >= n1)
  | _ -> a = b

let has_exact_type (ts : typ list) (target : typ) : bool = List.mem target ts

let rec is_concrete_type (t : typ) : bool =
  match t with
  | Tvar -> false
  | Tfunc (_, ret) | TfuncMin (_, ret) -> is_concrete_type ret
  | _ -> true

let has_list_any (ts : typ list) : bool = List.mem TlistAny ts

let exact_types_overlap (a : typ list) (b : typ list) : bool =
  List.exists (fun ta -> List.mem ta b) a

let concrete_types_overlap (a : typ list) (b : typ list) : bool =
  List.exists
    (fun ta ->
      is_concrete_type ta
      && List.exists
           (fun tb -> is_concrete_type tb && compatible_type ta tb)
           b)
    a

let primitive_symbol_types (name : string) : typ list =
  match name with
  | "+" | "*" -> [ TfuncMin (2, Tint) ]
  | "-" | "div" | "mod" -> [ Tfunc (2, Tint) ]
  | ">" | "<" | "<=" | ">=" | "eq" -> [ Tfunc (2, Tbool) ]
  | "not" | "atom" -> [ Tfunc (1, Tbool) ]
  | "and" | "or" -> [ Tfunc (2, Tbool) ]
  | "'" | "quote" -> [ Tfunc (1, Tvar) ]
  | "car" -> [ Tfunc (1, Tvar) ]
  | "cdr" -> [ Tfunc (1, TlistAny) ]
  | "cons" -> [ Tfunc (2, Tvar) ]
  | "cond" -> [ TfuncMin (1, Tvar) ]
  | name when is_cxr_name name -> [ Tfunc (1, Tvar) ]
  | _ -> []

let quoted_datum_types (e : exp) : typ list =
  match e with
  | A (Num _) -> [ Tint ]
  | A (Ident _) -> [ Tvar ]
  | A (Sym s) when Symbol.SymbolStr.tostr s = "t" -> [ Tbool ]
  | A (Sym s) when Symbol.SymbolStr.tostr s = "NIL" -> [ Tbool; Tlist 0 ]
  | A (Sym s) ->
      let name = Symbol.SymbolStr.tostr s in
      let symbol_types = primitive_symbol_types name in
      if symbol_types = [] then [] else symbol_types
  | L es -> [ Tlist (List.length es) ]

let is_quote_form (e : exp) : bool =
  match name_of_exp e with "'" | "quote" -> true | _ -> false

let is_quoted_atom_literal (e : exp) : bool =
  match e with
  | L [ q; A (Num _) ] when is_quote_form q -> true
  | L [ q; A (Ident _) ] when is_quote_form q -> true
  | L [ q; A (Sym _) ] when is_quote_form q -> true
  | _ -> false

let recursive_seed_return_types (_arity : int) : typ list =
  [ Tvar ]

let lookup_in_env (env : type_env) (name : string) : typ list =
  let rec find = function
    | [] -> []
    | (key, value) :: rest -> if key = name then value else find rest
  in
  find env

let rec bind_params (env : type_env) (params : exp list) : type_env =
  match params with
  | [] -> env
  | A (Ident id) :: rest ->
      (Variable.VarStr.tostr id, [ Tvar ]) :: bind_params env rest
  | _ :: rest -> bind_params env rest

let rec bind_params_to_arg_types (env : type_env) (params : exp list)
    (arg_types : typ list list) : type_env =
  match (params, arg_types) with
  | [], _ | _, [] -> env
  | A (Ident id) :: rest, arg_ts :: rest_args ->
      (Variable.VarStr.tostr id, arg_ts)
      :: bind_params_to_arg_types env rest rest_args
  | _ :: rest, _ :: rest_args -> bind_params_to_arg_types env rest rest_args

let params_are_valid = function
  | L ps -> List.for_all (function A (Ident _) -> true | _ -> false) ps
  | A (Sym sym) when Symbol.SymbolStr.tostr sym = "NIL" -> true
  | _ -> false

let param_list_of = function L ps -> ps | _ -> []
