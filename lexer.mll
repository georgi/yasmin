{
  open Parser
}

let letter = ['_' 'a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
  | [' ' '\t']                            { token lexbuf }
  | ['\n' '\r' ';']                       { NEWLINE }
      
  | "struct"                              { STRUCT }
  | "bool"                                { BOOL }
  | "byte"                                { BYTE }
  | "string"                              { STRING }
  | "int"                                 { INT }
  | "float"                               { FLOAT }
  | "void"                                { VOID }
  | "new"                                 { NEW }

  | ( letter ( letter | digit )* ) as lxm { IDENT(lxm) }
  | '"' ( [^ '"' '\n']* ) '"' as lxm      { STRING_LITERAL(String.sub lxm 1 ((String.length lxm) - 2)) }

  | ( digit* ) as lxm                     { INT_LITERAL(int_of_string lxm) }
  | ( digit ( digit | '.' )* ) as lxm     { FLOAT_LITERAL(float_of_string lxm) }

  | '#' [^ '\n']*                         { token lexbuf }

  | '('                                   { LPAREN }
  | ')'                                   { RPAREN }
  | '{'                                   { LCURLY }
  | '}'                                   { RCURLY }
  | '['                                   { LBRACK }
  | ']'                                   { RBRACK }
  | ','                                   { COMMA }
  | '.'                                   { DOT }
  | ':'                                   { COLON }
  | '='                                   { EQUALS }

  | '+'                                   { PLUS }
  | '-'                                   { MINUS }
  | '*'                                   { TIMES }

  | eof                                   { EOS }
  | _                                     { UNKNOWN }
