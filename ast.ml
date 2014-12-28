(*===----------------------------------------------------------------------===
 * Abstract Syntax Tree (aka Parse Tree)
 *===----------------------------------------------------------------------===*)

type type_def =
  | Bool
  | Byte
  | Int
  | Float
  | Void
  | Array of int * type_def
  | Function of type_def list * type_def
  | Undefined

type expr =
  | FloatLiteral of float
  | IntLiteral of int
  | Sequence of expr list * type_def
  | ArrayLiteral of expr list * type_def
  | Call of string * expr list * type_def
  | Def of string * expr * type_def
  | Fun of string list * type_def list * expr * type_def

type toplevel =
  | Expression of expr
  | Sep
  | End
