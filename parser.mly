
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

%token LET IN EXTERN INT BOOL BYTE FLOAT VOID RETURN
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
  | FLOAT_LITERAL                             { FloatLiteral $1 }
  | INT_LITERAL                               { IntLiteral $1 }

  | type_name IDENT EQUALS expr IN expr       { Let ($1, $2, $4, $6, Undefined) }

  | LPAREN expr RPAREN                        { $2 }
  | LPAREN expr                               { parse_error "expected ')'" }

  | expr LBRACK expr RBRACK                   { Call ("[]", [$1; $3], Undefined) }
  | expr LBRACK expr RBRACK EQUALS expr       { Call ("[]=", [$1; $3; $6], Undefined) }
  | IDENT LPAREN params RPAREN                { Call ($1, $3, Undefined) }
  | IDENT                                     { Var ($1, Undefined) }

  | expr PLUS expr                            { Call ("+", [$1; $3], Undefined) }
  | expr MINUS expr                           { Call ("-", [$1; $3], Undefined) }
  | expr TIMES expr                           { Call ("*", [$1; $3], Undefined) }

  | LCURLY sequence RCURLY                    { Sequence ($2, Undefined) }

  | fun_def                                   { $1 }

  | UNKNOWN { parse_error "unknown token when expecting an expression." }
;

fun_def:
  | type_name IDENT LPAREN args RPAREN LCURLY sequence RCURLY
      { Fun($2, List.map snd $4, List.map fst $4, Sequence ($7, Undefined), $1) }
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
;
args:
  | type_name IDENT COMMA args { ($1, $2) :: $4 }
  | type_name IDENT            { [($1, $2)] }
;
params:
  | expr COMMA params          { $1 :: $3 }
  | expr                       { [$1] }
;
toplevel:
  | statement terminator       { $1 }
  | terminator                 { $1 }
;
statement:
  | expr                       { Expression $1 }
;
terminator:
  | SEMICOLON                  { Sep }
  | EOS                        { End }
;
