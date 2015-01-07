{
  open Parser
}

let letter = ['_' 'a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
  | [' ' '\t']                            { token lexbuf }
  | ['\n' '\r']                           { token lexbuf }
      
  | "struct"                              { STRUCT }
  | "bool"                                { BOOL }
  | "byte"                                { BYTE }
  | "string"                              { STRING }
  | "int"                                 { INT }
  | "let"                                 { LET }
  | "float"                               { FLOAT }
  | "void"                                { VOID }
  | "new"                                 { NEW }
  | "if"                                  { IF }
  | "in"                                  { IN }
  | "then"                                { THEN }
  | "else"                                { ELSE }
  | "end"                                 { END }
  | "def"                                 { DEF }
  | "true"                                { TRUE }
  | "false"                               { FALSE }
  | "and"                                 { AND }
  | "or"                                  { OR }
  | "do"                                  { DO }
  | "fun"                                 { FUN }
  | "->"                                  { ARROW }

  | ( letter ( letter | digit )* ) as lxm { IDENT(lxm) }
  | '"' ( [^ '"' '\n']* ) '"' as lxm      { STRING_LITERAL(String.sub lxm 1 ((String.length lxm) - 2)) }
  | '\'' ( [^ '\'']+ ) '\'' as lxm        { CHAR_LITERAL(String.sub lxm 1 ((String.length lxm) - 2)) }

  | ( digit* ) as lxm                     { INT_LITERAL(int_of_string lxm) }
  | ( digit ( digit | '.' )* ) as lxm     { FLOAT_LITERAL(float_of_string lxm) }

  | '#' [^ '\n']*                         { token lexbuf }

  | '|'                                   { PIPE }
  | '('                                   { LPAREN }
  | ')'                                   { RPAREN }
  | '{'                                   { LCURLY }
  | '}'                                   { RCURLY }
  | '['                                   { LBRACK }
  | ']'                                   { RBRACK }
  | ','                                   { COMMA }
  | '.'                                   { DOT }
  | ':'                                   { COLON }
  | ';'                                   { SEMICOLON }
  | '='                                   { EQUALS }
  | '<'                                   { LESSTHAN }
  | '>'                                   { GREATERTHAN }
  | '!'                                   { BANG }
  | '+'                                   { PLUS }
  | '-'                                   { MINUS }
  | '*'                                   { TIMES }
  | '/'                                   { DIV }

  | eof                                   { EOS }
  | _                                     { UNKNOWN }
