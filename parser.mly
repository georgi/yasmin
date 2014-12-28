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

%token FUN EXTERN INT BOOL BYTE FLOAT VOID ARROW
%token <string> IDENT
%token <float> FLOAT_LITERAL
%token <int> INT_LITERAL
%token LPAREN RPAREN LCURLY RCURLY LBRACK RBRACK
%token COMMA SEMICOLON COLON EQUALS
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
  | LBRACK array RBRACK              { ArrayLiteral($2, Undefined) }

  | LPAREN expr RPAREN               { $2 }
  | LPAREN expr                      { parse_error "expected ')'" }

  | IDENT args                       { Call($1, $2, Undefined) }
  | IDENT                            { Call($1, [], Undefined) }

  | expr PLUS expr                   { Call("+", [$1; $3], Undefined) }
  | expr MINUS expr                  { Call("-", [$1; $3], Undefined) }
  | expr TIMES expr                  { Call("*", [$1; $3], Undefined) }

  | IDENT EQUALS expr                { Def($1, $3, Undefined) }
  | LCURLY sequence RCURLY           { Sequence($2, Undefined) }
  | FUN idents COLON types EQUALS expr { Fun($2, $4, $6, Undefined) }

  | UNKNOWN                          { parse_error "unknown token when expecting an expression." }
;
array:
  | expr COMMA array                 { $1 :: $3 }
  | expr                             { [$1] }
;
sequence:
  | expr SEMICOLON sequence          { $1 :: $3 }    
  | expr                             { [$1] }    
;
type_def:
  | BYTE                             { Byte }
  | INT                              { Int }
  | FLOAT                            { Float }
  | VOID                             { Void }
;
types:
  | type_def ARROW types             { $1 :: $3 }
  | type_def                         { [$1] }
;
idents:
  | IDENT idents                     { $1 :: $2 }
  | IDENT                            { [$1] }
;
args:
  | expr args                        { $1 :: $2 }
  | expr                             { [$1] }
;
toplevel:
  | statement terminator             { $1 }
  | terminator                       { $1 }
;
statement:
  | expr                             { Expression $1 }
;
terminator:
  | SEMICOLON                        { Sep }
  | EOS                              { End }
;
