type type_name =
  | Bool
  | Byte
  | Int16
  | Int32
  | Int
  | Float
  | Double
  | Void
  | String
  | TypeRef of string
  | Struct of (string * type_name) list
  | Pointer of type_name
  | Function of type_name list * type_name
  | Undefined

type expr =
  | FloatLiteral of float
  | IntLiteral of int
  | StringLiteral of string
  | StructLiteral of (string * expr) list * type_name
  | StructDef of string * (string * type_name) list
  | New of expr * type_name
  | Let of string * expr * type_name
  | Var of string * type_name
  | Mem of expr * string * type_name
  | MemSet of expr * string * expr * type_name
  | Call of string * expr list * type_name
  | Fun of string * string list * type_name list * expr list * type_name

type toplevel =
  | Expression of expr list
  | Sep
  | End
