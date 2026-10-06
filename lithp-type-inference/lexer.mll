{
open Grammar

exception Lexing_error of string
}

rule token = parse
  | [' ' '\t' '\n']+ { WS }
  | ";;;;"[^'\n']* as s {
      let content = String.trim (String.sub s 4 (String.length s - 4)) in
      FILE_HEADER content
    }
  | ";;;"[^'\n']* as s {
      let content = String.trim (String.sub s 3 (String.length s - 3)) in
      WHOLE_LINE_COMMENT content
    }
  | ";;"[^'\n']* as s {
      let content = String.trim (String.sub s 2 (String.length s - 2)) in
      INDENTED_COMMENT content
    }
  | ';'[^'\n']* as s {
      let content = String.trim (String.sub s 1 (String.length s - 1)) in
      INLINE_COMMENT content
    }
  | ';'[^'\n']* eof { EOF }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "'" { SQUOTE }
  | "()" { NIL }
  | "+" { PLUS }
  | "*" { MULT }
  | "-" { MINUS }
  | "div" { DIV }
  | "mod" { MOD }
  | "<=" { LE }
  | ">=" { GE }
  | "=/=" { NE }
  | "=" { EQ }
  | "<" { LT }
  | ">" { GT }
  | "quote" { K_QUOTE }
  | "atom" { K_ATOM }
  | "eq" { K_EQ }
  | "car" { K_CAR }
  | "cdr" { K_CDR }
  | "cons" { K_CONS }
  | "cond" { K_COND }
  | "lambda" { K_LAMBDA }
  | "label" { K_LABEL }
  | "defun" { K_DEFUN }
  | 'c' ['a' 'd']+ 'r' as s { CADR s }
  | ['0'-'9']+ as s { NUM (Bigint.BigNum.string_to_bigint s) }
  | "t" { TRUE }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* '.'? as s { IDENT s }
  | eof { EOF }
  | _ {
      let ch = Lexing.lexeme_char lexbuf 0 in
      raise (Lexing_error ("unrecognised character: " ^ String.make 1 ch))
    }