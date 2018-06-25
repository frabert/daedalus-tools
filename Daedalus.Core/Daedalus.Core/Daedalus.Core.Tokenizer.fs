namespace Daedalus.Core

open System.Text
open System
open FParsec
module Tokenizer =
  type Position = {
    line: uint64
    column: uint64 }

  type Operator =
    | Plus
    | Minus
    | Times
    | Divide
    | Modulo
    | GreaterThan
    | GreaterLess
    | LesserThan
    | LesserEqual
    | Equal
    | NotEqual
    | LogicalNot
    | BitwiseNot
    | Xor
    | LogicalAnd
    | BitwiseAnd
    | LogicalOr
    | BitwiseOr
    | ShiftLeft
    | ShiftRight
    | Assign
    | AssignPlus
    | AssignMinus
    | AssignTimes
    | AssignDivide
    | AssignModulo

  type Keyword =
    | Var
    | Const
    | Func
    | Instance
    | Prototype
    | Class
    | If
    | While
    | Return

  type TokenContent =
    | Keyword of Keyword * string
    | Identifier of string
    | Semicolon
    | OpenParens | ClosedParens
    | OpenBracket | ClosedBracket
    | OpenBrace | ClosedBrace
    | StringLiteral of string
    | IntegerLiteral of string
    | FloatLiteral of string
    | Operator of Operator
    | Comment of string
    | MultilineComment of string

  type Token = TokenContent * Position

  type private State =
    | Start
    | Slash
    | Comment
    | MultilineComment1
    | MultilineComment2
    | String
    | StringEscape
    | Number
    | Identifier
    | Operator
    | CarriageReturn

  let tokenize (str : string) =
    let incCol l p = { line = p.line; column = p.column + (uint64 l) }
    let adv = incCol 1
    let newLine p = { line = p.line + 1uL; column = 1uL }

    let rec aux chars state cur pos posStart =
      match state, chars with
        | Start, '\r' :: rest -> aux rest CarriageReturn "\r" pos pos
        | Start, '\n' :: rest -> aux rest Start cur (newLine pos) (newLine pos)
        | Start, c :: rest when Char.IsWhiteSpace(c) -> aux rest Start cur (adv pos) pos
        | Start, '_' :: rest -> aux rest Identifier "_" (adv pos) pos
        | Start, c :: rest when Char.IsLetter(c) -> aux rest Identifier (c.ToString()) (adv pos) pos
        | Start, c :: rest when Char.IsDigit(c) -> aux rest Number (c.ToString()) (adv pos) pos
        | Start, '/' :: rest -> aux rest Slash "/" (adv pos) pos
        | Start, '+' :: rest -> aux rest Operator "+" (adv pos) pos
        | Start, '-' :: rest -> aux rest Operator "-" (adv pos) pos
        | Start, '*' :: rest -> aux rest Operator "*" (adv pos) pos
        | Start, '%' :: rest -> aux rest Operator "%" (adv pos) pos
        | Start, '=' :: rest -> aux rest Operator "=" (adv pos) pos
        | Start, '"' :: rest -> aux rest String "\"" (adv pos) pos
        | Start, '(' :: rest -> (OpenParens, pos) :: aux rest Start "" (adv pos) (adv pos)
        | Start, ')' :: rest -> (ClosedParens, pos) :: aux rest Start "" (adv pos) (adv pos)
        | Start, '[' :: rest -> (OpenBracket, pos) :: aux rest Start "" (adv pos) (adv pos)
        | Start, ']' :: rest -> (ClosedBracket, pos) :: aux rest Start "" (adv pos) (adv pos)
        | Start, '{' :: rest -> (OpenBrace, pos) :: aux rest Start "" (adv pos) (adv pos)
        | Start, '}' :: rest -> (ClosedBrace, pos) :: aux rest Start "" (adv pos) (adv pos)
        | Start, ';' :: rest -> (Semicolon, pos) :: aux rest Start "" (adv pos) (adv pos)

        | CarriageReturn, '\n' :: rest -> aux rest Start "" (newLine pos) posStart
        | CarriageReturn, _ -> failwithf "Invalid sequence at %d:%d" pos.line pos.column

        | Identifier, '_' :: rest -> aux rest Identifier (cur + "_") (adv pos) posStart
        | Identifier, c :: rest when Char.IsLetterOrDigit(c) -> aux rest Identifier (cur + c.ToString()) (adv pos) posStart
        | Identifier, _ ->
          (match cur.ToLower() with
            | "var" -> (Keyword(Var, cur), posStart)
            | "const" -> (Keyword(Const, cur), posStart)
            | "func" -> (Keyword(Func, cur), posStart)
            | "instance" -> (Keyword(Instance, cur), posStart)
            | "prototype" -> (Keyword(Prototype, cur), posStart)
            | "class" -> (Keyword(Class, cur), posStart)
            | "if" -> (Keyword(If, cur), posStart)
            | "while" -> (Keyword(While, cur), posStart)
            | "return" -> (Keyword(Return, cur), posStart)
            | _ -> (TokenContent.Identifier cur, posStart)) :: aux chars Start "" pos pos 

        | Number, c :: rest when Char.IsDigit(c) -> aux rest Number (cur + c.ToString()) (adv pos) posStart
        | Number, _ -> (IntegerLiteral cur, posStart) :: aux chars Start "" pos pos

        | Slash, '/' :: rest -> aux rest Comment "//" (adv pos) posStart
        // TODO: multiline comments

        | Operator, '=' :: rest ->
          (match cur with
            | "+" -> (TokenContent.Operator AssignPlus, posStart)
            | "-" -> (TokenContent.Operator AssignMinus, posStart)
            | "*" -> (TokenContent.Operator AssignTimes, posStart)
            | "%" -> (TokenContent.Operator AssignModulo, posStart)
            | "=" -> (TokenContent.Operator Equal, posStart)
            | _ -> failwith "invalid state reached") :: aux rest Start "" (adv pos) (adv pos)
        | Operator, _ ->
          ((match cur with
            | "+" -> TokenContent.Operator Plus
            | "-" -> TokenContent.Operator Minus
            | "*" -> TokenContent.Operator Times
            | "%" -> TokenContent.Operator Modulo
            | _ -> failwith "invalid state reached"), posStart) :: aux chars Start "" (adv pos) (adv pos)

        | String, '\\' :: rest -> aux rest StringEscape (cur + "\\") (adv pos) posStart
        | String, '"' :: rest -> (StringLiteral (cur + "\""), posStart) :: aux rest Start "" (adv pos) (adv pos)
        | String, c :: rest -> aux rest String (cur + c.ToString()) (adv pos) posStart
        | String, [] -> failwithf "Unterminated string at %d:%d" pos.line pos.column

        | Comment, '\r' :: rest -> (TokenContent.Comment cur, posStart) :: aux rest CarriageReturn "\r" (adv pos) (adv pos)
        | Comment, '\n' :: rest -> (TokenContent.Comment cur, posStart) :: aux rest Start "" (newLine pos) (newLine pos)
        | Comment, c :: chars -> aux chars Comment (cur + c.ToString()) (adv pos) posStart
        
        | StringEscape, c :: rest -> aux rest String (cur + c.ToString()) (adv pos) posStart

        | _, [] -> if cur.Length > 0 then failwith "unfinished token" else []


    aux (Seq.toList str) Start "" { line = 1uL; column = 1uL; } { line = 1uL; column = 1uL; }
