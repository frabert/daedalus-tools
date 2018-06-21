namespace Daedalus.Core

open Daedalus.Core.Ast
open FParsec

module Grammar =
  let expr, exprRef = createParserForwardedToRef<Expression, unit>()
  let stmt, stmtRef = createParserForwardedToRef<Statement, unit>()

  let comment =
    choice [
      between (pstring "/*") (pstring "*/") (many anyChar |>> ignore)
      pstring "//" >>. restOfLine true |>> ignore
    ] .>> spaces

  let ws = spaces >>. many comment |>> ignore
  let pstring_ws s = pstringCI s .>> ws
  let pchar_ws c = pchar c .>> ws

  let comma = pchar_ws ','
  let semicolon = pchar_ws ';'

  let ident =

    let isAsciiIdStart c =
      isAsciiLetter c || c = '_'
    let isAsciiIdContinue c =
      isAsciiLetter c || isDigit c || c = '_' || c = '\''

    identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart,
                               isAsciiIdContinue = isAsciiIdContinue)) .>> ws

  let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

  let variable =
    getPosition >>= fun pos -> ident |>> fun name -> Variable.Variable(name, pos)

  let typeName =
    getPosition >>= fun pos -> ident |>> fun name -> Type(name, pos)

  let exprList = sepBy expr comma

  let parenthesis a = between (pchar_ws '(') (pchar_ws ')') a
  let brackets a = between (pchar_ws '[') (pchar_ws ']') a
  let braces a = between (pchar_ws '{') (pchar_ws '}') a

  let baseExpr = 
    getPosition >>=
    fun pos -> choice [
      parenthesis expr
      puint32 |>> fun v -> IntLiteral(v, pos)
      pfloat |>> fun v -> FloatLiteral(v, pos)
      stringLiteral |>> fun s -> StringLiteral(s, pos)
      variable >>= fun v ->
        choice [
          // var_a.var_b
          pchar '.' >>. variable |>> fun m -> MemberAccess(v, m, pos)

          // var_a(e1, e2, e3...)
          parenthesis exprList |>>
            fun args -> FuncCall(v, args, pos)

          // var_a
          Variable(v) |> preturn
        ] >>= fun b ->
          choice [
            // some.thing[expr]
            brackets expr |>>
              fun idx -> ArrayAccess(b, idx, pos)

            // some.thing
            preturn b
          ]
    ] .>> ws

  let opExpr =
    let opp = new OperatorPrecedenceParser<Expression,_,_>()

    opp.AddOperator(PrefixOperator("+", getPosition .>> ws, 100, true, (),
                      fun pos e -> Unary(UnaryOp.Plus, e, pos)))
    opp.AddOperator(PrefixOperator("-", getPosition .>> ws, 100, true, (),
                      fun pos e -> Unary(UnaryOp.Minus, e, pos)))
    opp.AddOperator(PrefixOperator("!", getPosition .>> ws, 100, true, (),
                      fun pos e -> Unary(UnaryOp.LogicalNot, e, pos)))
    opp.AddOperator(PrefixOperator("~", getPosition .>> ws, 100, true, (),
                      fun pos e -> Unary(UnaryOp.BitwiseNot, e, pos)))

    opp.AddOperator(InfixOperator("*", getPosition .>> ws, 90, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(Times, e1, e2, pos)))
    opp.AddOperator(InfixOperator("/", getPosition .>> ws, 90, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(Divide, e1, e2, pos)))
    opp.AddOperator(InfixOperator("%", getPosition .>> ws, 90, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(Modulo, e1, e2, pos)))

    opp.AddOperator(InfixOperator("+", getPosition .>> ws, 80, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(Plus, e1, e2, pos)))
    opp.AddOperator(InfixOperator("-", getPosition .>> ws, 80, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(Minus, e1, e2, pos)))

    opp.AddOperator(InfixOperator(">>", getPosition .>> ws, 70, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(ShiftRight, e1, e2, pos)))
    opp.AddOperator(InfixOperator("<<", getPosition .>> ws, 70, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(ShiftLeft, e1, e2, pos)))

    opp.AddOperator(InfixOperator("<", getPosition .>> ws, 60, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(Less, e1, e2, pos)))
    opp.AddOperator(InfixOperator(">", getPosition .>> ws, 60, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(Greater, e1, e2, pos)))
    opp.AddOperator(InfixOperator("<=", getPosition .>> ws, 60, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(LessEqual, e1, e2, pos)))
    opp.AddOperator(InfixOperator(">=", getPosition .>> ws, 60, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(GreaterEqual, e1, e2, pos)))


    opp.AddOperator(InfixOperator("==", getPosition .>> ws, 50, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(Equal, e1, e2, pos)))
    opp.AddOperator(InfixOperator("!=", getPosition .>> ws, 50, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(NotEqual, e1, e2, pos)))

    opp.AddOperator(InfixOperator("&", getPosition .>> ws, 40, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BitwiseAnd, e1, e2, pos)))
                      
    opp.AddOperator(InfixOperator("^", getPosition .>> ws, 30, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BitwiseXor, e1, e2, pos)))

    opp.AddOperator(InfixOperator("|", getPosition .>> ws, 20, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BitwiseOr, e1, e2, pos)))

    opp.AddOperator(InfixOperator("&&", getPosition .>> ws, 10, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(LogicalAnd, e1, e2, pos)))

    opp.AddOperator(InfixOperator("||", getPosition .>> ws, 5, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(LogicalOr, e1, e2, pos)))

    opp.AddOperator(InfixOperator("+=", getPosition .>> ws, 0, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BinaryOp.AssignPlus, e1, e2, pos)))
    opp.AddOperator(InfixOperator("-=", getPosition .>> ws, 0, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BinaryOp.AssignMinus, e1, e2, pos)))
    opp.AddOperator(InfixOperator("*=", getPosition .>> ws, 0, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BinaryOp.AssignTimes, e1, e2, pos)))
    opp.AddOperator(InfixOperator("/=", getPosition .>> ws, 0, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BinaryOp.AssignDivide, e1, e2, pos)))
    opp.AddOperator(InfixOperator("%=", getPosition .>> ws, 0, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BinaryOp.AssignModulo, e1, e2, pos)))
    opp.AddOperator(InfixOperator("=", getPosition .>> ws, 0, Associativity.Left, (),
                      fun pos e1 e2 -> Binary(BinaryOp.Assign, e1, e2, pos)))

    opp.TermParser <- baseExpr
    opp.ExpressionParser

  do exprRef := (opExpr .>> ws) <?> "expression"

  let block =
    getPosition >>= fun pos ->
      braces (many stmt) |>> fun body ->
        Block(body, pos)

  let ifStmt =
    let branch = getPosition >>= fun pos ->
      pstring_ws "if" >>. expr .>>. block |>>
        fun (cond, body) -> IfBranch(cond, body, pos)

    getPosition >>= fun pos ->
      sepBy1 branch (pstring_ws "else") .>>. (opt (pstring_ws "else" >>. block)) |>>
        fun (branches, _else) ->
          If(branches, _else, pos)

  let whileStmt =
    getPosition >>= fun pos ->
      pstring_ws "while" >>. expr .>>. block |>>
        fun (cond, body) -> While(cond, body, pos)

  let returnStmt =
    getPosition >>= fun pos ->
      pstring_ws "return" >>. opt expr .>> semicolon |>>
        fun e -> Return(e, pos)

  let exprStmt =
    getPosition >>= fun pos ->
      expr .>> semicolon |>> fun e -> Expression(e, pos)

  let vardeclStmt =
    let var_const = choice [
      pstring_ws "var" >>. preturn false
      pstring_ws "const" >>. preturn true
    ]

    getPosition >>= fun pos ->
      tuple3 var_const typeName variable >>= fun (isConst, _type, _var) ->
        let typedVar = { isConst = isConst; varType = _type; var = _var; position = pos }
        choice [
          comma >>. sepBy variable comma |>>
            fun vars -> MultivarDeclaration(isConst, _type, _var :: vars, pos)

          brackets expr .>>.
            (opt (pchar_ws '=' >>. braces exprList)) |>>
              fun (idx, init) -> Statement.ArrayDeclaration(typedVar, idx, init, pos)

          opt (pchar_ws '=' >>. expr) |>> fun e -> Statement.VariableDeclaration(typedVar, e, pos)
        ] .>> semicolon

  let assignment =
    let lhs = getPosition >>=
              fun pos -> variable >>=
                          fun _base -> choice [
                            pchar_ws '.' >>. variable |>>
                              fun m -> Expression.MemberAccess(_base, m, pos)

                            brackets expr |>>
                              fun e -> ArrayAccess(Variable(_base), e, pos)

                            Variable(_base) |> preturn
                          ]

    let op : Parser<_,_> = choice [
      pchar_ws '='    >>. preturn Assign
      pstring_ws "+=" >>. preturn AssignPlus
      pstring_ws "-=" >>. preturn AssignMinus
      pstring_ws "*=" >>. preturn AssignTimes
      pstring_ws "/=" >>. preturn AssignDivide
      pstring_ws "%=" >>. preturn AssignModulo
    ]
    getPosition >>= fun pos -> tuple3 lhs op expr |>> fun (l, o, r) -> Assignment(o, l, r, pos)
      .>> semicolon

  do stmtRef :=
      (choice [
        ifStmt
        whileStmt
        returnStmt
        vardeclStmt
        attempt assignment
        exprStmt
      ] <?> "statement") .>> many semicolon

  let funDecl =
    let param = getPosition >>= fun pos ->
      pstring_ws "var" >>. typeName .>>. variable |>>
      fun (_type, name) -> { isConst = false; varType = _type; var = name; position = pos }

    let paramList = parenthesis (sepBy param comma)

    getPosition >>= fun pos ->
      pstring_ws "func" >>. tuple4 typeName variable paramList block |>>
        fun (retType, name, parameters, body) -> Function({
          returnType = retType
          name = name
          parameters = parameters
          body = body
        }, pos)

  let prototype =
    getPosition >>= fun pos ->
      pstring_ws "prototype" >>. tuple3 variable (parenthesis variable) block |>>
        fun (name, parent, body) -> Prototype({
          name = name
          baseClass = parent
          body = body
        }, pos)

  let instance =
    getPosition >>= fun pos ->
      pstring_ws "instance" >>.
      variable >>= fun varName -> choice [
        comma >>.
          sepBy1 variable comma .>>.
          parenthesis typeName .>> semicolon |>>
          fun (vars, _type) -> InstanceVariableDeclaration(_type, varName :: vars, pos)

        parenthesis typeName >>= fun _type ->
          choice [
            semicolon >>. (InstanceVariableDeclaration(_type, [varName], pos) |> preturn)
            block |>> fun body -> Instance({
              instanceType = _type;
              name = varName;
              body = body;
            }, pos)
          ]
      ]
  let externClass =
    getPosition >>= fun pos ->
      pstring_ws "class" >>. variable .>>. block |>>
        fun (name, body) -> ExternClass(name, body, pos)

  let globalDecl =
    let varDecl = 
      let var_const = choice [
        pstring_ws "var" >>. preturn false
        pstring_ws "const" >>. preturn true
      ]

      getPosition >>= fun pos ->
        tuple3 var_const typeName variable >>= fun (isConst, _type, _var) ->
          let typedVar = { isConst = isConst; varType = _type; var = _var; position = pos }
          choice [
            brackets expr .>>.
              (opt (pchar_ws '=' >>. braces exprList)) |>>
                fun (idx, init) -> GlobalDeclaration.ArrayDeclaration(typedVar, idx, init, pos)

            opt (pchar_ws '=' >>. expr) |>> fun e -> GlobalDeclaration.VariableDeclaration(typedVar, e, pos)
          ] .>> semicolon

    (choice [
      funDecl
      prototype
      instance
      externClass
      varDecl
    ] <?> "global declaration") .>> many semicolon

  let program = many globalDecl