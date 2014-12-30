type type_name =
  | Bool
  | Byte
  | Int
  | Float
  | Void
  | Array of type_name * int
  | Function of type_name list * type_name
  | Undefined

type expr =
  | FloatLiteral of float
  | IntLiteral of int
  (* | ArrayLiteral of expr list * type_name *)
  | Return of expr
  | Let of type_name * string * expr * expr * type_name
  | Sequence of expr list * type_name
  | Var of string * type_name
  | Call of string * expr list * type_name
  | Fun of string * string list * type_name list * expr * type_name

type toplevel =
  | Expression of expr
  | FunDef of expr
  | Sep
  | End
