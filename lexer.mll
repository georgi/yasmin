(*===----------------------------------------------------------------------===
 * Lexer
 *===----------------------------------------------------------------------===*)
{
  open Parser
}
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
rule token = parse
  (* Skip any whitespace. *)
  | [' ' '\t']                            { token lexbuf }
  | ['\n' '\r' ]
      { Lexing.new_line lexbuf; token lexbuf }

  | "def"                                 { DEF }
  | "extern"                              { EXTERN }
  | "bool"                                { BOOL }
  | "byte"                                { BYTE }
  | "int"                                 { INT }
  | "float"                               { FLOAT }
  | "void"                                { VOID }

  (* identifier: [a-zA-Z][a-zA-Z0-9]* *)
  | ( letter ( letter | digit )* ) as lxm { IDENT(lxm) }

  (* number: [0-9.]+ *)
  | ( digit* ) as lxm                     { INT_LITERAL(int_of_string lxm) }
  | ( digit ( digit | '.' )* ) as lxm     { FLOAT_LITERAL(float_of_string lxm) }

  (* Comment until end of line. *)
  | '#' [^ '\n']*                         { token lexbuf }

  (* Parens *)
  | '('                                   { LPAREN }
  | ')'                                   { RPAREN }

  (* Punctuation *)
  | ','                                   { COMMA }
  | ':'                                   { COLON }
  | ';'                                   { SEMICOLON }
  | '='                                   { EQUALS }
  | '{'                                   { LCURLY }
  | '}'                                   { RCURLY }

  (* binary operators *)
  | '+'                                   { PLUS }
  | '-'                                   { MINUS }
  | '*'                                   { TIMES }

  (* end of stream. *)
  | eof                                   { EOS }

  | _                                     { UNKNOWN }
