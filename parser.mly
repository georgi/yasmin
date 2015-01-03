
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

%token NEW STRUCT INT BOOL BYTE FLOAT STRING VOID DEF TRUE FALSE AND OR FUN 
%token IF THEN ELSE END LET IN
%token <string> IDENT
%token <string> STRING_LITERAL
%token <float> FLOAT_LITERAL
%token <int> INT_LITERAL
%token LPAREN RPAREN LCURLY RCURLY LBRACK RBRACK
%token COMMA SEMICOLON COLON EQUALS DOT
%token LESSTHAN GREATERTHAN BANG 
%token EOS
%token UNKNOWN
%token PLUS MINUS TIMES DIV

%left PLUS
%left MINUS
%left DIV
%left TIMES

%start toplevel
%type <Ast.toplevel> toplevel
%%

expr:
  | TRUE                                      { True }
  | FALSE                                     { False }
  | FLOAT_LITERAL                             { FloatLiteral $1 }
  | INT_LITERAL                               { IntLiteral $1 }
  | STRING_LITERAL                            { StringLiteral $1 }
  | LBRACK array RBRACK                       { ArrayLiteral ($2, Undefined) }

  | expr PLUS expr                            { Call ("+", [$1; $3], Undefined) }

  | expr LBRACK expr RBRACK                   { Call ("[]", [$1; $3], Undefined) }
  | expr LBRACK expr RBRACK EQUALS expr       { Call ("[]=", [$1; $3; $6], Undefined) }

  | IDENT params_parens                       { Call ($1, $2, Undefined) }
  | IDENT                                     { Var ($1, Undefined) }

  | LPAREN expr RPAREN                        { $2 }
  | LPAREN expr                               { parse_error "expected ')'" }

  | expr DOT IDENT                            { Mem ($1, $3, Undefined) }
  | expr DOT IDENT EQUALS expr                { MemSet ($1, $3, $5, Undefined) }
  | expr MINUS expr                           { Call ("-", [$1; $3], Undefined) }
  | expr TIMES expr                           { Call ("*", [$1; $3], Undefined) }
  | expr DIV expr                             { Call ("/", [$1; $3], Undefined) }
  | expr EQUALS EQUALS expr                   { Call ("==", [$1; $4], Undefined) }
  | expr BANG EQUALS expr                     { Call ("!=", [$1; $4], Undefined) }
  | expr LESSTHAN expr                        { Call ("<", [$1; $3], Undefined) }
  | expr GREATERTHAN expr                     { Call (">", [$1; $3], Undefined) }
  | expr LESSTHAN EQUALS expr                 { Call ("<=", [$1; $4], Undefined) }
  | expr GREATERTHAN EQUALS expr              { Call (">=", [$1; $4], Undefined) }
  | expr AND expr                             { Call ("and", [$1; $3], Undefined) }
  | expr OR expr                              { Call ("or", [$1; $3], Undefined) }

  | NEW type_name                             { New (IntLiteral 1, $2) }
  | NEW type_name LBRACK expr RBRACK          { New ($4, $2) }

  | struct_literal                            { $1 }
  | if_then_else                              { $1 }
  | FUN args_parens expr                      { Fun (List.map snd $2, List.map fst $2, $3, Undefined) }
  | LET IDENT EQUALS expr IN expr             { Let ($2, $4, $6, Undefined) }

  | UNKNOWN { parse_error "unknown token when expecting an expression." }
;
if_then_else:
  | IF expr THEN expr ELSE expr { If ($2, $4, $6, Undefined) }
;
struct_literal:
  | LCURLY lmembers RCURLY     { StructLiteral ($2, Undefined) }
;
lmember:
  | IDENT COLON expr           { ($1, $3) }
;
lmembers:
  | lmember COMMA lmembers     { $1 :: $3 }
  | lmember COMMA              { [$1] }
  | lmember                    { [$1] }
;
array:
  | expr COMMA array           { $1 :: $3 }
  | expr                       { [$1] }
;
type_name:
  | BYTE                       { Byte }
  | STRING                     { String }
  | INT                        { Int }
  | FLOAT                      { Float }
  | VOID                       { Void }
  | IDENT                      { TypeRef $1 }
;
args_parens:
  | LPAREN args RPAREN         { $2 }
  | LPAREN RPAREN              { [] }
;
args:
  | type_name IDENT COMMA args { ($1, $2) :: $4 }
  | type_name IDENT            { [($1, $2)] }
;
params_parens:
  | LPAREN params RPAREN       { $2 }
  | LPAREN RPAREN              { [] }
;
params:
  | expr COMMA params          { $1 :: $3 }
  | expr                       { [$1] }
;
fun_def:
  | DEF IDENT args_parens expr { FunDef ($2, List.map snd $3, List.map fst $3, $4) }
;
struct_def:
  | STRUCT IDENT LCURLY members RCURLY { StructDef ($2, $4) }
;
member:
  | type_name IDENT            { ($2, $1) }
;
members:
  | member SEMICOLON members   { $1 :: $3 }
  | member SEMICOLON           { [$1] }
;
toplevel:
  | statement terminator       { $1 }
  | terminator                 { $1 }
;
statement:
  | expr                       { Expression $1 }
  | fun_def                    { $1 }
  | struct_def                 { $1 }
;
terminator:
  | SEMICOLON                  { Sep }
  | EOS                        { End }
;
