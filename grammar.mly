%{
open Ast

(* A file is represented as one top-level program node whose children are the
   expressions that appear in sequence. *)

let attach_head_arity (es : exp list) : exp list =
  match es with
  | A (Sym s) :: args ->
      A (Sym (Symbol.SymbolStr.mk_sym (Symbol.SymbolStr.tostr s) (List.length args)))
      :: args
  | _ -> es
%}

%token LPAREN RPAREN SQUOTE EOF TRUE WS
%token NIL PLUS MULT MINUS DIV MOD LE GE NE EQ LT GT
%token K_QUOTE K_ATOM K_EQ K_CAR K_CDR K_CONS K_COND K_LAMBDA K_LABEL K_DEFUN
%token <string> CADR IDENT
%token <Bigint.BigNum.bigint> NUM
%token <string> FILE_HEADER WHOLE_LINE_COMMENT INDENTED_COMMENT INLINE_COMMENT

%start program
%type <Ast.program> program

%%

program:
  | sep_opt EOF { Program [] }
  | sep_opt exprs sep_opt EOF { Program (List.rev $2) }
;

exprs:
  | expr { [$1] }
  | exprs sep expr { $3 :: $1 }
  | exprs sep { $1 }
;

sep_opt:
  | { [] }
  | sep_opt sep_unit { [] }
;

sep:
  | sep_unit { [] }
  | sep sep_unit { [] }
;

sep_unit:
  | WS { [] }
  | comment { [] }
;

comment:
  | FILE_HEADER { Comment $1 }
  | WHOLE_LINE_COMMENT { Comment $1 }
  | INDENTED_COMMENT { Comment $1 }
  | INLINE_COMMENT { Comment $1 }
;

expr:
  | SQUOTE sep_opt expr {
      L [A (Sym (Symbol.SymbolStr.mk_sym "quote" 1)); $3]
    }
  | atom { A $1 }
  | list { $1 }
;

atom:
  | NUM { Num $1 }
  | IDENT { Ident (Variable.VarStr.mk_var $1) }
  | CADR { Sym (Symbol.SymbolStr.mk_sym $1 0) }

  | TRUE { Sym (Symbol.SymbolStr.mk_sym "t" 0) }
  | NIL { Sym (Symbol.SymbolStr.mk_sym "NIL" 0) }

  | PLUS { Sym (Symbol.SymbolStr.mk_sym "+" 0) }
  | MINUS { Sym (Symbol.SymbolStr.mk_sym "-" 0) }
  | MULT { Sym (Symbol.SymbolStr.mk_sym "*" 0) }
  | EQ { Sym (Symbol.SymbolStr.mk_sym "=" 0) }
  | GT { Sym (Symbol.SymbolStr.mk_sym ">" 0) }
  | LT { Sym (Symbol.SymbolStr.mk_sym "<" 0) }
  | LE { Sym (Symbol.SymbolStr.mk_sym "<=" 0) }
  | GE { Sym (Symbol.SymbolStr.mk_sym ">=" 0) }
  | NE { Sym (Symbol.SymbolStr.mk_sym "=/=" 0) }
  | DIV { Sym (Symbol.SymbolStr.mk_sym "div" 0) }
  | MOD { Sym (Symbol.SymbolStr.mk_sym "mod" 0) }

  | K_QUOTE { Sym (Symbol.SymbolStr.mk_sym "quote" 0) }
  | K_ATOM { Sym (Symbol.SymbolStr.mk_sym "atom" 0) }
  | K_EQ { Sym (Symbol.SymbolStr.mk_sym "eq" 0) }
  | K_CAR { Sym (Symbol.SymbolStr.mk_sym "car" 0) }
  | K_CDR { Sym (Symbol.SymbolStr.mk_sym "cdr" 0) }
  | K_CONS { Sym (Symbol.SymbolStr.mk_sym "cons" 0) }
  | K_COND { Sym (Symbol.SymbolStr.mk_sym "cond" 0) }
  | K_LAMBDA { Sym (Symbol.SymbolStr.mk_sym "lambda" 0) }
  | K_LABEL { Sym (Symbol.SymbolStr.mk_sym "label" 0) }
  | K_DEFUN { Sym (Symbol.SymbolStr.mk_sym "defun" 0) }
;

list:
  | LPAREN sep_opt elements_opt sep_opt RPAREN {
      let list_items = List.rev $3 in
      match list_items with
      (* Empty list becomes NIL. *)
      | [] -> A (Sym (Symbol.SymbolStr.mk_sym "NIL" 0))
      | _ -> L (attach_head_arity list_items)
    }
;

elements_opt:
  | { [] }
  | elements { $1 }
;

elements:
  | expr { [$1] }
  | elements sep expr { $3 :: $1 }
;
