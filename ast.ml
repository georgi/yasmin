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
  | Array of type_name
  | Pointer of type_name
  | Function of type_name list * type_name
  | Undefined
  
type expr =
  | True
  | False
  | FloatLiteral of float
  | IntLiteral of int
  | StringLiteral of string
  | ArrayLiteral of expr list * type_name
  | StructLiteral of (string * expr) list * type_name
  | New of expr * type_name
  | Let of string * expr * type_name
  | Var of string * type_name
  | Mem of expr * string * type_name
  | MemSet of expr * string * expr * type_name
  | Call of string * expr list * type_name
  | If of expr * expr list * expr list * type_name
  | Fun of string list * type_name list * expr list * type_name

type toplevel =
  | Expression of expr list
  | StructDef of string * (string * type_name) list
  | FunDef of string * string list * type_name list * expr list
  | Sep
  | End
