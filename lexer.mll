{
  open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let quote = '"'

rule token = parse
  (* Skip any whitespace. *)
  | [' ' '\t']                            { token lexbuf }

  | ['\n' '\r' ]                          { NEWLINE }
      
  | "bool"                                { BOOL }
  | "byte"                                { BYTE }
  | "int"                                 { INT }
  | "float"                               { FLOAT }
  | "void"                                { VOID }
  | "new"                                 { NEW }

  (* identifier: [a-zA-Z][a-zA-Z0-9]* *)
  | ( letter ( letter | digit )* ) as lxm { IDENT(lxm) }
  | quote ( [^ '\n']* ) quote as lxm      { STRING_LITERAL(String.sub lxm 1 ((String.length lxm) - 2)) }

  (* number: [0-9.]+ *)
  | ( digit* ) as lxm                     { INT_LITERAL(int_of_string lxm) }
  | ( digit ( digit | '.' )* ) as lxm     { FLOAT_LITERAL(float_of_string lxm) }

  (* Comment until end of line. *)
  | '#' [^ '\n']*                         { token lexbuf }

  (* Parens *)
  | '('                                   { LPAREN }
  | ')'                                   { RPAREN }
  | '{'                                   { LCURLY }
  | '}'                                   { RCURLY }
  | '['                                   { LBRACK }
  | ']'                                   { RBRACK }

  (* Punctuation *)
  | ','                                   { COMMA }
  | ':'                                   { COLON }
  | ';'                                   { SEMICOLON }
  | '='                                   { EQUALS }

  (* binary operators *)
  | '+'                                   { PLUS }
  | '-'                                   { MINUS }
  | '*'                                   { TIMES }

  (* end of stream. *)
  | eof                                   { EOS }

  | _                                     { UNKNOWN }
