open Ast

let parse lexbuf =
  try Grammar.program Lexer.token lexbuf with
  | Lexer.Lexing_error msg -> failwith ("Lexing error: " ^ msg)
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      failwith (Printf.sprintf "Parse error at line %d, column %d" line col)

let string_of_atom = function
  | Num n -> "Num(" ^ Bigint.BigNum.pretty_print n ^ ")"
  | Ident id -> "Ident(" ^ Variable.VarStr.tostr id ^ ")"
  | Sym sym ->
      "Sym(" ^ Symbol.SymbolStr.tostr sym ^ ","
      ^ string_of_int (Symbol.SymbolStr.arity sym)
      ^ ")"

let rec string_of_typ = function
  | Tint -> "Int"
  | Tbool -> "Bool"
  | Tvar -> "Any"
  | Tlist n -> "List(" ^ string_of_int n ^ ")"
  | TlistAny -> "List(any)"
  | Tfunc (arity, typ) ->
      "List(" ^ string_of_int arity ^ ") -> " ^ string_of_typ typ
  | TfuncMin (arity, typ) ->
      "List(n), n >= " ^ string_of_int arity ^ " -> " ^ string_of_typ typ

let string_of_type_set types =
  match types with
  | [] -> "TypeError"
  | [ typ ] -> string_of_typ typ
  | _ -> "[" ^ String.concat "; " (List.map string_of_typ types) ^ "]"

let rec string_of_exp = function
  | A atom -> "A[" ^ string_of_atom atom ^ "]"
  (* Semicolons make nested list output much easier to read while debugging. *)
  | L exps -> "L[" ^ String.concat "; " (List.map string_of_exp exps) ^ "]"

let string_of_program (Program exps : program) =
  "Program[" ^ String.concat "; " (List.map string_of_exp exps) ^ "]"

let with_input_channel path f =
  if path = "" then f stdin
  else
    let ic = open_in path in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)

let () =
  let input_path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  with_input_channel input_path (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      let program = parse lexbuf in
      Type.typeof_program program
      |> List.map string_of_type_set
      |> String.concat "\n" |> print_endline)
