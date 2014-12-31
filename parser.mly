
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

%token NEW IN EXTERN INT BOOL BYTE FLOAT VOID
%token <string> IDENT
%token <string> STRING_LITERAL
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
  | FLOAT_LITERAL                             { FloatLiteral $1 }
  | INT_LITERAL                               { IntLiteral $1 }
  | STRING_LITERAL                            { StringLiteral $1 }

  | LPAREN expr RPAREN                        { $2 }
  | LPAREN expr                               { parse_error "expected ')'" }

  | expr LBRACK expr RBRACK                   { Call ("[]", [$1; $3], Undefined) }
  | expr LBRACK expr RBRACK EQUALS expr       { Call ("[]=", [$1; $3; $6], Undefined) }
  | IDENT params_parens                       { Call ($1, $2, Undefined) }
  | IDENT                                     { Var ($1, Undefined) }

  | expr PLUS expr                            { Call ("+", [$1; $3], Undefined) }
  | expr MINUS expr                           { Call ("-", [$1; $3], Undefined) }
  | expr TIMES expr                           { Call ("*", [$1; $3], Undefined) }

  | type_name IDENT EQUALS expr IN sequence   { Let ($1, $2, $4, $6, Undefined) }

  | NEW type_name                             { New (IntLiteral 1, $2) }
  | NEW type_name LBRACK expr RBRACK          { New ($4, $2) }

  | fun_def                                   { $1 }

  | UNKNOWN { parse_error "unknown token when expecting an expression." }
;

fun_def:
  | type_name IDENT args_parens LCURLY sequence RCURLY
      { Fun($2, List.map snd $3, List.map fst $3, $5, $1) }
;
array:
  | expr COMMA array           { $1 :: $3 }
  | expr                       { [$1] }
;
sequence:
  | expr SEMICOLON sequence    { $1 :: $3 }    
  | expr SEMICOLON             { [$1] }    
;
type_name:
  | BYTE                       { Byte }
  | INT                        { Int }
  | FLOAT                      { Float }
  | VOID                       { Void }
  | type_name TIMES            { Pointer $1 }    
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
toplevel:
  | expr SEMICOLON             { Expression $1 }
  | expr EOS                   { Expression $1 }
;
