namespace Daedalus.Core.Ast

type Type = Type of string * FParsec.Position
type Variable = Variable of string * FParsec.Position
type TypedVariable = { 
  isConst: bool;
  varType: Type;
  var: Variable;
  position: FParsec.Position }

type UnaryOp =
  | Plus
  | Minus
  | LogicalNot
  | BitwiseNot

type BinaryOp =
  | Assign
  | AssignPlus
  | AssignMinus
  | AssignTimes
  | AssignDivide
  | AssignModulo
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | ShiftLeft
  | ShiftRight
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | BitwiseAnd
  | BitwiseXor
  | BitwiseOr
  | LogicalAnd
  | LogicalOr

type AssignOp =
  | Assign
  | AssignPlus
  | AssignMinus
  | AssignTimes
  | AssignDivide
  | AssignModulo

type Expression =
  | IntLiteral of uint32 * FParsec.Position
  | StringLiteral of string * FParsec.Position
  | FloatLiteral of float * FParsec.Position
  | Variable of Variable
  | MemberAccess of Variable * Variable * FParsec.Position
  | FuncCall of Variable * Expression list * FParsec.Position
  | Unary of UnaryOp * Expression * FParsec.Position
  | Binary of BinaryOp * Expression * Expression * FParsec.Position
  | ArrayAccess of Expression * Expression * FParsec.Position
  | Parenthesis of Expression * FParsec.Position

type IfBranch = IfBranch of cond:Expression * body:Block * FParsec.Position
and Block = Block of Statement list * FParsec.Position
and Statement =
  | VariableDeclaration of TypedVariable * Expression option * FParsec.Position
  | MultivarDeclaration of isConst:bool * varType:Type * variables:Variable list * FParsec.Position
  | ArrayDeclaration of variable:TypedVariable * size:Expression * rhs:Expression list option * FParsec.Position
  | Assignment of AssignOp * Expression * Expression * FParsec.Position
  | Return of Expression option * FParsec.Position
  | Expression of Expression * FParsec.Position
  | If of IfBranch list * Block option * FParsec.Position
  | While of Expression * Block * FParsec.Position

type Function = {
  returnType: Type;
  name: Variable;
  parameters: TypedVariable list;
  body: Block }

type Prototype = {
  name: Variable;
  baseClass: Variable;
  body: Block; }

type Instance = {
  instanceType: Type;
  name: Variable;
  body: Block; }

type GlobalDeclaration =
  | VariableDeclaration of TypedVariable * Expression option * FParsec.Position
  | ArrayDeclaration of variable:TypedVariable * size:Expression * rhs:Expression list option * FParsec.Position
  | InstanceVariableDeclaration of typeName:Type * vars:Variable list * FParsec.Position
  | Function of Function * FParsec.Position
  | Prototype of Prototype * FParsec.Position
  | Instance of Instance * FParsec.Position
  | ExternClass of name:Variable * body:Block * FParsec.Position

type Program = GlobalDeclaration list