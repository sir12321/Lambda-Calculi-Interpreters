open Ast
open Type_support

let primitive_type (env : type_env)
    (typeof_exp_with_env : type_env -> exp -> typ list)
    (type_match_with_env : type_env -> exp list -> typ -> bool) (fexp : exp)
    (args : exp list) : typ list =
  match (name_of_exp fexp, args) with
  | ("+" | "*"), x :: y :: xs
    when has_exact_type (typeof_exp_with_env env x) Tint
         && has_exact_type (typeof_exp_with_env env y) Tint
         && type_match_with_env env xs Tint ->
      [ Tint ]
  | ("-" | "div" | "mod"), [ x; y ]
    when has_exact_type (typeof_exp_with_env env x) Tint
         && has_exact_type (typeof_exp_with_env env y) Tint ->
      [ Tint ]
  | (">" | "<" | "<=" | ">="), [ x; y ]
    when has_exact_type (typeof_exp_with_env env x) Tint
         && has_exact_type (typeof_exp_with_env env y) Tint ->
      [ Tbool ]
  | "not", [ x ] when has_exact_type (typeof_exp_with_env env x) Tbool -> [ Tbool ]
  | ("and" | "or"), [ x; y ]
    when has_exact_type (typeof_exp_with_env env x) Tbool
         && has_exact_type (typeof_exp_with_env env y) Tbool ->
      [ Tbool ]
  | ("'" | "quote"), [ x ] -> quoted_datum_types x
  | "atom", [ _ ] -> [ Tbool ]
  | "eq", [ x; y ]
    when exact_types_overlap
           (typeof_exp_with_env env x)
           (typeof_exp_with_env env y)
         || (is_quoted_atom_literal x && is_quoted_atom_literal y)
    ->
      [ Tbool ]
  | "car", [ x ]
    when let x_types = typeof_exp_with_env env x in
         let lens = list_lengths x_types in
         List.exists (fun n -> n > 0) lens
         || has_exact_type x_types Tvar
         || has_list_any x_types -> (
      match x with
      | L [ quoted_head; y ] -> (
          match name_of_exp quoted_head with
          | "'" | "quote" -> (
              match y with L (h :: _) -> quoted_datum_types h | _ -> [])
          | _ -> [ Tvar ])
      | _ -> [ Tvar ])
  | "cdr", [ x ] ->
      let x_types = typeof_exp_with_env env x in
      let inferred =
        list_lengths x_types
        |> List.fold_left
             (fun acc n ->
               if n > 0 then add_type_if_missing acc (Tlist (n - 1)) else acc)
             []
      in
      let inferred =
        if has_exact_type x_types Tvar || has_list_any x_types then
          add_type_if_missing inferred TlistAny
        else inferred
      in
      if inferred <> [] then inferred
      else if is_quoted_atom_literal x then [ TlistAny ]
      else []
  | name, [ x ] when is_cxr_name name ->
      typeof_exp_with_env env (expand_cxr name x)
  | "cons", [ _; y ] ->
      let y_types = typeof_exp_with_env env y in
      let inferred =
        list_lengths y_types
        |> List.fold_left
             (fun acc n -> add_type_if_missing acc (Tlist (n + 1)))
             []
      in
      if has_exact_type y_types Tvar then add_type_if_missing inferred Tvar
      else if has_list_any y_types then add_type_if_missing inferred TlistAny
      else inferred
  | "cond", clauses when clauses <> [] ->
      let rec type_cond_clauses (cs : exp list) : typ list =
        match cs with
        | [] -> []
        | [ L [ test_e; branch_e ] ] when has_exact_type (typeof_exp_with_env env test_e) Tbool ->
            typeof_exp_with_env env branch_e
        | L [ test_e; branch_e ] :: rest
          when has_exact_type (typeof_exp_with_env env test_e) Tbool ->
            let branch_types = typeof_exp_with_env env branch_e in
            let rest_types = type_cond_clauses rest in
            if branch_types = [] || rest_types = [] then [] else union_types branch_types rest_types
        | _ -> []
      in
      type_cond_clauses clauses
  | "lambda", [ params; body ] when params_are_valid params ->
      let param_list = param_list_of params in
      let arity = List.length param_list in
      let env' = bind_params env param_list in
      let body_types = typeof_exp_with_env env' body in
      let body_types = if body_types = [] then [ Tvar ] else body_types in
      List.map (fun ret_ty -> Tfunc (arity, ret_ty)) body_types
  | "label", [ A (Ident f_id); lambda_exp ] -> (
      match lambda_exp with
      | L (lambda_head :: params :: _)
        when name_of_exp lambda_head = "lambda" && params_are_valid params ->
          let arity = List.length (param_list_of params) in
          let fname = Variable.VarStr.tostr f_id in
          let seed =
            List.map
              (fun ret_ty -> Tfunc (arity, ret_ty))
              (recursive_seed_return_types arity)
          in
          let env' = (fname, seed) :: env in
          let inferred =
            List.filter
              (function Tfunc _ -> true | TfuncMin _ -> true | _ -> false)
              (typeof_exp_with_env env' lambda_exp)
          in
          if inferred = [] then seed else inferred
      | _ -> [])
  | "defun", [ A (Ident f_id); params; body ] when params_are_valid params ->
      let lambda_sym = A (Sym (Symbol.SymbolStr.mk_sym "lambda" 2)) in
      let label_sym = A (Sym (Symbol.SymbolStr.mk_sym "label" 2)) in
      let lambda_exp = L [ lambda_sym; params; body ] in
      typeof_exp_with_env env (L [ label_sym; A (Ident f_id); lambda_exp ])
  | _ -> []
