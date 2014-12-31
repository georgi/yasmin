type type_name =
  | Bool
  | Byte
  | Int16
  | Int32
  | Int
  | Float
  | Void
  | Pointer of type_name
  | Function of type_name list * type_name
  | Undefined

type expr =
  | FloatLiteral of float
  | IntLiteral of int
  | StringLiteral of string
  | New of expr * type_name
  | Let of string * expr * type_name
  | Var of string * type_name
  | Call of string * expr list * type_name
  | Fun of string * string list * type_name list * expr list * type_name * bool

type toplevel =
  | Expression of expr
  | Sep
  | End
