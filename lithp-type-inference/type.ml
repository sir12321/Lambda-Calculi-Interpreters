open Ast
open Type_support

type defun_def = { name : string; params : exp list; body : exp }
type defun_env = defun_def list
type call_stack = string list

let symbol_arity_matches (fexp : exp) (args : exp list) : bool =
  match fexp with
  | A (Sym s) -> Symbol.SymbolStr.arity s = List.length args
  | _ -> true

let is_lambda_like (e : exp) : bool =
  match e with
  | L [ A (Sym s); _; _ ] when Symbol.SymbolStr.tostr s = "lambda" -> true
  | L [ A (Sym s); A (Ident _); _ ] when Symbol.SymbolStr.tostr s = "label" ->
      true
  | _ -> false

let defun_binding_of_exp (exp : exp) (types : typ list) :
    (string * typ list) list =
  match exp with
  | L [ A (Sym s); A (Ident f_id); _; _ ]
    when Symbol.SymbolStr.tostr s = "defun" ->
      [ (Variable.VarStr.tostr f_id, types) ]
  | _ -> []

let defun_of_exp (exp : exp) : defun_def option =
  match exp with
  | L [ A (Sym s); A (Ident f_id); params; body ]
    when Symbol.SymbolStr.tostr s = "defun" && params_are_valid params ->
      Some
        {
          name = Variable.VarStr.tostr f_id;
          params = param_list_of params;
          body;
        }
  | _ -> None

let lookup_defun (defs : defun_env) (name : string) : defun_def option =
  List.find_opt (fun def -> def.name = name) defs

let function_return_types_for_arity (ftypes : typ list) (arity : int) : typ list =
  List.fold_left
    (fun acc t ->
      match t with
      | Tfunc (n, ret_ty) when n = arity -> add_type_if_missing acc ret_ty
      | TfuncMin (n, ret_ty) when arity >= n -> add_type_if_missing acc ret_ty
      | _ -> acc)
    [] ftypes

let function_types_of_returns (arity : int) (returns : typ list) : typ list =
  List.map (fun ret_ty -> Tfunc (arity, ret_ty)) returns

let normalize_function_returns (returns : typ list) : typ list =
  if returns = [] then [ Tvar ] else returns

let recursive_seed_candidates : typ list =
  [ Tvar; Tint; Tbool; TlistAny; Tlist 0 ]

let rec infer_recursive_body_return_types (defs : defun_env) (stack : call_stack)
    (base_env : type_env) (name : string) (params : exp list) (body : exp)
    (arg_types : typ list list) : typ list =
  let arity = List.length params in
  let rec stabilize guess fuel =
    let env' =
      (name, function_types_of_returns arity guess)
      :: bind_params_to_arg_types base_env params arg_types
    in
    let inferred =
      typeof_exp_with_ctx defs (name :: stack) env' body |> normalize_function_returns
    in
    if inferred = guess || fuel = 0 then inferred else stabilize inferred (fuel - 1)
  in
  let inferred =
    List.fold_left
      (fun acc seed ->
        let stable = stabilize [ seed ] 8 in
        union_types acc stable)
      [] recursive_seed_candidates
  in
  let concrete =
    List.filter (function Tvar -> false | _ -> true) inferred
  in
  if concrete <> [] then concrete else normalize_function_returns inferred

and fallback_function_application_type (defs : defun_env) (stack : call_stack)
    (env : type_env) (fexp : exp) (args : exp list) : typ list =
  let arity = List.length args in
  let ftypes = typeof_exp_with_ctx defs stack env fexp in
  function_return_types_for_arity ftypes arity

and typeof_exp_with_ctx (defs : defun_env) (stack : call_stack) (env : type_env)
    (e : exp) : typ list =
  match e with
  | A (Num _) -> [ Tint ]
  | A (Ident id) ->
      let type_from_env = lookup_in_env env (Variable.VarStr.tostr id) in
      if type_from_env = [] then [ Tvar ] else type_from_env
  | A (Sym s) when Symbol.SymbolStr.tostr s = "t" -> [ Tbool ]
  | A (Sym s) when Symbol.SymbolStr.tostr s = "NIL" -> [ Tbool; Tlist 0 ]
  | A (Sym s) ->
      let primitive_types = primitive_symbol_types (Symbol.SymbolStr.tostr s) in
      if primitive_types = [] then [] else primitive_types
  | L [] -> [ Tbool; Tlist 0 ]
  | L (fexp :: args) when not (symbol_arity_matches fexp args) -> []
  | L (fexp :: args) ->
      let fname = name_of_exp fexp in
      if is_primitive_name fname then
        Type_rules.primitive_type env (typeof_exp_with_ctx defs stack)
          (type_match_with_ctx defs stack) fexp args
      else
        let specialized = infer_lambda_application_type defs stack env fexp args in
        if specialized <> [] then specialized
        else if is_lambda_like fexp then []
        else fallback_function_application_type defs stack env fexp args

and infer_lambda_application_type (defs : defun_env) (stack : call_stack)
    (env : type_env) (fexp : exp) (args : exp list) : typ list =
  let arg_types = List.map (typeof_exp_with_ctx defs stack env) args in
  match fexp with
  | L [ A (Sym s); params; body ]
    when Symbol.SymbolStr.tostr s = "lambda"
         && params_are_valid params
         && List.length (param_list_of params) = List.length args ->
      let env' =
        bind_params_to_arg_types env (param_list_of params) arg_types
      in
      typeof_exp_with_ctx defs stack env' body
  | L [ A (Sym s); A (Ident f_id); lambda_exp ]
    when Symbol.SymbolStr.tostr s = "label" -> (
      match lambda_exp with
      | L [ A (Sym lambda_sym); params; body ]
        when Symbol.SymbolStr.tostr lambda_sym = "lambda"
             && params_are_valid params
             && List.length (param_list_of params) = List.length args ->
          let fname = Variable.VarStr.tostr f_id in
          infer_recursive_body_return_types defs stack env fname
            (param_list_of params) body arg_types
      | _ -> [])
  | A (Ident f_id) -> (
      match lookup_defun defs (Variable.VarStr.tostr f_id) with
      | Some { name; params; body } when List.length params = List.length args ->
          if List.mem name stack then function_return_types_for_arity (lookup_in_env env name) (List.length args)
          else
            infer_recursive_body_return_types defs stack env name params body arg_types
      | _ -> [])
  | _ -> []

and type_match_with_ctx (defs : defun_env) (stack : call_stack) (env : type_env)
    (e : exp list)
    (t : typ) : bool =
  match (e, t) with
  | [], _ -> true
  | A (Num _) :: es, Tint -> type_match_with_ctx defs stack env es Tint
  | A (Sym s) :: es, Tbool when Symbol.SymbolStr.tostr s = "t" ->
      type_match_with_ctx defs stack env es Tbool
  | A (Sym s) :: es, Tbool when Symbol.SymbolStr.tostr s = "NIL" ->
      type_match_with_ctx defs stack env es Tbool
  | x :: es, Tint
    when has_exact_type (typeof_exp_with_ctx defs stack env x) Tint ->
      type_match_with_ctx defs stack env es Tint
  | x :: es, Tbool
    when has_exact_type (typeof_exp_with_ctx defs stack env x) Tbool ->
      type_match_with_ctx defs stack env es Tbool
  | _ -> false

let typeof_exp_with_env (env : type_env) (e : exp) : typ list =
  typeof_exp_with_ctx [] [] env e

let type_match_with_env (env : type_env) (e : exp list) (t : typ) : bool =
  type_match_with_ctx [] [] env e t

let typeof (e : exp) : typ list = typeof_exp_with_ctx [] [] [] e
let type_match (e : exp list) (t : typ) : bool = type_match_with_ctx [] [] [] e t

let typeof_program (Program exps : program) : typ list list =
  let rec infer_program defs env es acc =
    match es with
    | [] -> List.rev acc
    | exp :: rest ->
        let inferred = typeof_exp_with_ctx defs [] env exp in
        let env' = defun_binding_of_exp exp inferred @ env in
        let defs' =
          match defun_of_exp exp with Some def -> def :: defs | None -> defs
        in
        infer_program defs' env' rest (inferred :: acc)
  in
  infer_program [] [] exps []
