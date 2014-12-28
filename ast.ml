(*===----------------------------------------------------------------------===
 * Abstract Syntax Tree (aka Parse Tree)
 *===----------------------------------------------------------------------===*)

type type_def =
  | Bool
  | Byte
  | Int
  | Float
  | Void
  | Undefined

type arg_def = string * type_def

type proto = string * arg_def list * type_def

type expr =
  | FloatLiteral of float
  | IntLiteral of int
  | Sequence of expr list
  | Variable of string * type_def 
  | Call of string * expr list * type_def
  | Function of proto * expr

type toplevel =
  | Expression of expr
  | Extern of proto
  | Sep
  | End
