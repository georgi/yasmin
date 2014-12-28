%{
  open Ast
  open Lexing
  let parse_error s =
    begin
      try
        let start_pos = Parsing.symbol_start_pos ()
        and end_pos = Parsing.symbol_end_pos () in
        Printf.printf "File \"%s\", line %d, characters %d-%d: \n"
          start_pos.pos_fname
          start_pos.pos_lnum
          (start_pos.pos_cnum - start_pos.pos_bol)
          (end_pos.pos_cnum - start_pos.pos_bol)
      with Invalid_argument(_) -> ()
    end;
    Printf.printf "Syntax error: %s\n" s;
    raise Parsing.Parse_error
%}

%token DEF EXTERN INT BOOL BYTE FLOAT VOID
%token <string> IDENT
%token <float> FLOAT_LITERAL
%token <int> INT_LITERAL
%token LPAREN RPAREN
%token COMMA SEMICOLON COLON
%token EOS
%token UNKNOWN
%token PLUS MINUS TIMES

%left TIMES

%start toplevel
%type <Ast.toplevel> toplevel
%%

expr:
  | FLOAT_LITERAL                    { FloatLiteral $1 }
  | INT_LITERAL                      { IntLiteral $1 }

  | LPAREN expr RPAREN               { $2 }
  | LPAREN expr                      { parse_error "expected ')'" }

  | IDENT LPAREN arg_expr RPAREN     { Call($1, $3, Undefined) }
  | IDENT LPAREN RPAREN              { Call($1, [], Undefined) }
  | IDENT LPAREN arg_expr            { parse_error "expected ')'" }
  | IDENT                            { Variable($1, Undefined) }

  | expr PLUS expr                   { Call("+", [$1; $3], Undefined) }
  | expr MINUS expr                  { Call("-", [$1; $3], Undefined) }
  | expr TIMES expr                  { Call("*", [$1; $3], Undefined) }

  | DEF prototype expr               { Function($2, $3) }

  | UNKNOWN                          { parse_error "unknown token when expecting an expression." }
;
type_def:
  | BOOL                             { Bool }
  | BYTE                             { Byte }
  | INT                              { Int }
  | FLOAT                            { Float }
  | VOID                             { Void }
;
arg_expr:
  | expr COMMA arg_expr              { $1 :: $3 }
  | expr                             { [$1] }
;
prototype:
  | IDENT LPAREN arg_defs RPAREN     { ($1, $3, Undefined) }
  | IDENT LPAREN arg_defs            { parse_error "expected ')' in prototype" }
  | IDENT                            { parse_error "expected '(' in prototype" }
;
arg_def:
  | IDENT COLON type_def             { ($1, $3) }
;
arg_defs:
  | arg_def COMMA arg_defs           { $1 :: $3 }
  | arg_def                          { [$1] }
  |                                  { [] }
;

extern:
  | EXTERN prototype                 { $2 }
;

toplevel:
  | statement terminator             { $1 }
  | terminator                       { $1 }
;
statement:
  | expr                             { Expression $1 }
  | extern                           { Extern $1 }
;
terminator:
  | SEMICOLON                        { Sep }
  | EOS                              { End }
;
