package redscript.compiler

import redscript.compiler.ast._

import scala.language.{implicitConversions, postfixOps}
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader

class Parser(val source: String) extends StdTokenParsers
{
    override type Tokens  = TokenSpace
    override val  lexical = new Tokenizer

    /* helper methods */

    private def ^[T](p: Parser[T]): Parser[T] = commit(p)
    private def line[T](p: Parser[T]): Parser[T] = p <~ (lexical.NewLine *)
    private def tail[T](p: Parser[T]): Parser[T] = not(lexical.NewLine) ~> p <~ guard(lexical.NewLine *)

    /* implicit converters for parsing expressions */

    private implicit def expr2node(v: NodeExpr): Either[NodeExpr, NodeValue] = Left(v)
    private implicit def value2node(v: NodeValue): Either[NodeExpr, NodeValue] = Right(v)

    /* implicit converters for `parseTuple` */

    private implicit def value2target(v: NodeValue): List[Either[NodeValue, NodeLTuple]] = Left(v) :: Nil
    private implicit def tuple2target(v: NodeLTuple): List[Either[NodeValue, NodeLTuple]] = Right(v) :: Nil

    /* literals */

    private lazy val parseIntConst      : Parser[NodeIntConst]      = positioned(accept("int"   , { case lexical.IntLit(value)    => new NodeIntConst(value)    }))
    private lazy val parseLongConst     : Parser[NodeLongConst]     = positioned(accept("int"   , { case lexical.LongLit(value)   => new NodeLongConst(value)   }))
    private lazy val parseFloatConst    : Parser[NodeFloatConst]    = positioned(accept("float" , { case lexical.FloatLit(value)  => new NodeFloatConst(value)  }))
    private lazy val parseStringConst   : Parser[NodeStringConst]   = positioned(accept("string", { case lexical.StringLit(value) => new NodeStringConst(value) }))

    /* expressions */

    private lazy val parseExpr          : Parser[NodeExpr]          = positioned(parsePair      ) * (line("->") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "->", y)})
    private lazy val parsePair          : Parser[NodeExpr]          = positioned(parseBoolOr    ) * (line("in") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "in", y)})
    private lazy val parseBoolOr        : Parser[NodeExpr]          = positioned(parseBoolAnd   ) * (line("||") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "||", y)})
    private lazy val parseBoolAnd       : Parser[NodeExpr]          = positioned(parseBitOr     ) * (line("&&") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "&&", y)})
    private lazy val parseBitOr         : Parser[NodeExpr]          = positioned(parseBitXor    ) * (line("|" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "|" , y)})
    private lazy val parseBitXor        : Parser[NodeExpr]          = positioned(parseBitAnd    ) * (line("^" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "^" , y)})
    private lazy val parseBitAnd        : Parser[NodeExpr]          = positioned(parseEquals    ) * (line("&" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "&" , y)})
    private lazy val parseEquals        : Parser[NodeExpr]          = positioned(parseCompares  ) * (line("!=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "!=", y)}
                                                                                                  |  line("==") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "==", y)})
    private lazy val parseCompares      : Parser[NodeExpr]          = positioned(parseShifts    ) * (line("<" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<" , y)}
                                                                                                  |  line(">" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">" , y)}
                                                                                                  |  line("<=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<=", y)}
                                                                                                  |  line(">=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">=", y)})
    private lazy val parseShifts        : Parser[NodeExpr]          = positioned(parseAddSub    ) * (line("<<") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<<", y)}
                                                                                                  |  line(">>") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">>", y)})
    private lazy val parseAddSub        : Parser[NodeExpr]          = positioned(parseTerm      ) * (line("+" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "+" , y)}
                                                                                                  |  line("-" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "-" , y)})
    private lazy val parseTerm          : Parser[NodeExpr]          = positioned(parsePower     ) * (line("*" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "*" , y)}
                                                                                                  |  line("/" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "/" , y)}
                                                                                                  |  line("%" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "%" , y)})
    private lazy val parsePower         : Parser[NodeExpr]          = positioned(parseFactor    ) * (line("**") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "**", y)})

    /* values */

    private lazy val parseFactor        : Parser[NodeExpr]          =
        ( "+" ~> ^(positioned(parseFactor)) ^^ (new NodeExpr(null, "+", _))
        | "-" ~> ^(positioned(parseFactor)) ^^ (new NodeExpr(null, "-", _))
        | "~" ~> ^(positioned(parseFactor)) ^^ (new NodeExpr(null, "~", _))
        | "!" ~> ^(positioned(parseFactor)) ^^ (new NodeExpr(null, "!", _))
        |          positioned(parseRValue)  ^^ (new NodeExpr(_, null, null)))

    private lazy val parseModOp         : Parser[String]            = guard(line(".") | tail("[") | tail("("))
    private lazy val parseRValue        : Parser[NodeValue]         = positioned(parseBaseValue) ~ parseRModifiers ^^ { case value ~ rmods => new NodeValue(value, rmods, isRValue = true ) }
    private lazy val parseLValue        : Parser[NodeValue]         =
        ( positioned(parseSetName) <~ not(parseModOp)   ^^ (new NodeValue(_, Nil, isRValue = false))
        | positioned(parseBaseValue) ~ parseLModifiers  ^^ { case value ~ lmods => new NodeValue(value, lmods, isRValue = false) })

    private lazy val parseBaseValue     : Parser[AST]               =
        ( positioned(parseIntConst      )
        | positioned(parseLongConst     )
        | positioned(parseFloatConst    )
        | positioned(parseStringConst   )
        | positioned(parseLambda        )
        | positioned(parseRange         )
        | positioned(parseArray         )
        | positioned(parseMap           )
        | positioned(parseGetName       )
        | line("(") ~> ^(positioned(parseExpr)) <~ ^(line(")")))

    private lazy val parseLModifiers    : Parser[List[AST]]         = (
        ( positioned(parseInvokeStack   )
        | positioned(parseInvokeAttr    )
        | positioned(parseGetIndex      ) <~ parseModOp
        | positioned(parseGetAttr       ) <~ parseModOp) *
        ) ~
        ( positioned(parseSetAttr   )
        | positioned(parseSetIndex  )
        ) ^^ { case invokes ~ tail => invokes :+ tail }

    private lazy val parseRModifiers    : Parser[List[AST]]         =
        ( positioned(parseInvokeStack   )
        | positioned(parseInvokeAttr    )
        | positioned(parseGetIndex      )
        | positioned(parseGetAttr       )) *

    private lazy val parseMap           : Parser[NodeMap]           = line("{") ~> repsep(positioned(parseItem), line(",")) <~ ^(line("}"))       ^^ (new NodeMap(_))
    private lazy val parseArray         : Parser[NodeArray]         = line("[") ~> repsep(positioned(parseExpr), line(",")) <~ ^(line("]"))       ^^ (new NodeArray(_))
    private lazy val parseInvokeStack   : Parser[NodeInvokeStack]   = tail("(") ~> repsep(positioned(parseActualArgs), line(",")) <~ ^(line(")")) ^^ (new NodeInvokeStack(_))

    private lazy val parseRange         : Parser[NodeRange]         =
        (line("{" )  ~>   positioned(parseExpr)  <~
         line("..")) ~ (^(positioned(parseExpr)) <~ line("}")) ^^ {
            case lower ~ upper => new NodeRange(lower, upper)
        }

    private lazy val parseLambda        : Parser[NodeFunctionDef]   =
        (line("(") ~> repsep(positioned(parseFormalArgs), line(",")) <~ line(")")) ~
        (line("=>") ~> ^(positioned(parseStatement))) ^^ {
            case args ~ body => new NodeFunctionDef(null, args, body)
        }

    private lazy val parseActualArgs    : Parser[NodeActualArg]     =
        ( line(">") ~> ^(positioned(parseExpr)) <~ (^(guard(")")) withErrorMessage "expansion must be the last one of actual arguments")   ^^ (new NodeActualArg(_, expand = true))
        |                positioned(parseExpr)                                                                                             ^^ (new NodeActualArg(_, expand = false)))

    private lazy val parseFormalArgs    : Parser[NodeFormalArg]     =
        ( line(">") ~> ^(positioned(parseSetName)) <~ (^(guard(")")) withErrorMessage "variadic must be the last one of formal arguments") ^^ (new NodeFormalArg(_, variadic = true))
        |                positioned(parseSetName)                                                                                          ^^ (new NodeFormalArg(_, variadic = false)))

    private lazy val parseItem          : Parser[NodeMapItem]       = positioned(parseExpr) ~ (^(line(":")) ~> ^(positioned(parseExpr)))         ^^ { case key ~ value => new NodeMapItem(key, value) }
    private lazy val parseInvokeAttr    : Parser[NodeInvokeAttr]    = line(".") ~> ^(positioned(parseGetName)) ~ ^(positioned(parseInvokeStack)) ^^ { case attr ~ invoke => new NodeInvokeAttr(attr, invoke) }

    private lazy val parseGetName       : Parser[NodeName]          = positioned(ident ^^ (new NodeName(_, isGet = true)))
    private lazy val parseSetName       : Parser[NodeName]          = positioned(ident ^^ (new NodeName(_, isGet = false)))

    private lazy val parseGetAttr       : Parser[NodeAttr]          = line(".") ~> ^(positioned(parseGetName))                  ^^ (new NodeAttr(_, isGet = true))
    private lazy val parseGetIndex      : Parser[NodeIndex]         = tail("[") ~> ^(positioned(parseExpr   )) <~ ^(line("]"))  ^^ (new NodeIndex(_, isGet = true))

    private lazy val parseSetAttr       : Parser[NodeAttr]          = line(".") ~> ^(positioned(parseGetName))                  ^^ (new NodeAttr(_, isGet = false))
    private lazy val parseSetIndex      : Parser[NodeIndex]         = tail("[") ~> ^(positioned(parseExpr   )) <~ ^(line("]"))  ^^ (new NodeIndex(_, isGet = false))
    private lazy val parseGetSubTuple   : Parser[NodeRTuple]        = line("(") ~>   positioned(parseGetTuple) <~ ^(line(")"))
    private lazy val parseSetSubTuple   : Parser[NodeLTuple]        = line("(") ~>   positioned(parseSetTuple) <~ ^(line(")"))

    private lazy val parseGetTuple      : Parser[NodeRTuple]         =
        ( positioned(parseGetSubTuple   ) <~ "," ^^ (Left(_))
        | positioned(parseExpr          ) <~ "," ^^ (Right(_))
        ) ~
        repsep( positioned(parseGetSubTuple ) ^^ (Left(_))
              | positioned(parseExpr        ) ^^ (Right(_)), ",") ^^ {
            case first ~ rest => new NodeRTuple(first :: rest)
        }

    private lazy val parseSetTuple      : Parser[NodeLTuple]         =
        ( positioned(parseSetSubTuple   ) <~ "," ^^ (Left(_))
        | positioned(parseLValue        ) <~ "," ^^ (Right(_))
        ) ~
        repsep( positioned(parseSetSubTuple ) ^^ (Left(_))
              | positioned(parseLValue      ) ^^ (Right(_)), ",") ^^ {
            case first ~ rest => new NodeLTuple(first :: rest)
        }

    /* statements */

    private lazy val parseCompond       : Parser[NodeCompond]       = line("{") ~> (positioned(parseStatement) *) <~ ^(line("}")) ^^ (new NodeCompond(_))
    private lazy val parseStatement     : Parser[NodeStatement]     =
        ( positioned(parseIf            ) ^^ (new NodeStatement(_))
        | positioned(parseFor           ) ^^ (new NodeStatement(_))
        | positioned(parseTry           ) ^^ (new NodeStatement(_))
        | positioned(parseBreak         ) ^^ (new NodeStatement(_))
        | positioned(parseRaise         ) ^^ (new NodeStatement(_))
        | positioned(parseWhile         ) ^^ (new NodeStatement(_))
        | positioned(parseReturn        ) ^^ (new NodeStatement(_))
        | positioned(parseSwitch        ) ^^ (new NodeStatement(_))
        | positioned(parseCompond       ) ^^ (new NodeStatement(_))
        | positioned(parseContinue      ) ^^ (new NodeStatement(_))
        | positioned(parseIncrement     ) ^^ (new NodeStatement(_))
        | positioned(parseAssignment    ) ^^ (new NodeStatement(_))
        | positioned(parseFunctionDef   ) ^^ (new NodeStatement(_))
        | positioned(parseExpr          ) ^^ (new NodeStatement(_))
        ) <~ (";" | guard(lexical.EOF) | (lexical.NewLine *))

    /* language structures */

    private lazy val parseBreak         : Parser[NodeBreak]         = line("break"   ) ^^^ new NodeBreak
    private lazy val parseContinue      : Parser[NodeContinue]      = line("continue") ^^^ new NodeContinue

    private lazy val parseRaise         : Parser[NodeRaise]         = line("raise") ~> ^(positioned(parseExpr)) ^^ (new NodeRaise(_))
    private lazy val parseReturn        : Parser[NodeReturn]        = line("return") ~>
        ^(positioned(parseGetTuple  ) ^^ (Left(_))
        | positioned(parseExpr      ) ^^ (Right(_))) ^^ (new NodeReturn(_))

    private lazy val parseIncOp         : Parser[String]            = line("+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | ">>=")
    private lazy val parseIncrement     : Parser[NodeIncrement]     = positioned(parseLValue) ~ parseIncOp ~ ^(positioned(parseExpr)) ^^ { case target ~ op ~ expr => new NodeIncrement(target, op, expr) }
    private lazy val parseAssignment    : Parser[NodeAssignment]    = ((
        ( positioned(parseSetTuple  ) ^^ (Left(_))
        | positioned(parseLValue    ) ^^ (Right(_))
        ) <~ line("=")) +) ~
        ^(positioned(parseGetTuple  ) ^^ (Left(_))
        | positioned(parseExpr      ) ^^ (Right(_))
        ) ^^ {
            case targets ~ expr => new NodeAssignment(targets, expr)
        }

    private lazy val parseIf            : Parser[NodeIf]            =
        ((line("if"  )  ~> ^(positioned(parseExpr     ))) ~
        ^(line("then")) ~  ^(positioned(parseStatement))  ~
        ((line("else")  ~> ^(positioned(parseStatement))) ?)) ^^ {
            case expr ~ _ ~ success ~ failed => new NodeIf(expr, success, failed)
        }

    private lazy val parseFor           : Parser[NodeFor]           = line("for") ~>
        ( positioned(parseSetTuple) ^^ (Left(_))
        | positioned(parseLValue  ) ^^ (Right(_))) ~
        ( line("in") ~> positioned(parseExpr)) ~
        ( line("do") ~> positioned(parseStatement)) ^^ {
            case target ~ expr ~ body => new NodeFor(target, expr, body)
        }

    private lazy val parseTry           : Parser[NodeTry]           =
        (line("try") ~> ^(positioned(parseStatement))) ~
        (^(guard(line("except") | line("finally"))) withErrorMessage "``except'' or ``finally'' blocks expected") ~
        ((line("except") ~> ^(line("{")) ~> ^(positioned(parseExceptBlock) +) <~ ^(line("}")) <~
        (^(guard(not(line("except")))) withErrorMessage "duplicated ``except'' block")) ?) ~
        ((line("finally") ~> ^(positioned(parseStatement)) <~
        (^(guard(not(line("except")))) withErrorMessage "``finally'' must be placed after the ``except'' block") ~
        (^(guard(not(line("finally")))) withErrorMessage "duplicated ``finally'' block")) ?) ^^ {
            case body ~ _ ~ None          ~ Some(finalizer) => new NodeTry(body, Nil, Some(finalizer))
            case body ~ _ ~ Some(excepts) ~      finalizer  => new NodeTry(body, excepts,  finalizer)
        }

    private lazy val parseExceptType    : Parser[List[NodeName]]    = rep1sep(positioned(parseGetName), line("."))
    private lazy val parseExceptName    : Parser[Option[NodeName]]  = (positioned(parseSetName) <~ line(":")) ?

    private lazy val parseExceptBlock   : Parser[NodeExcept]        =
        (line("case") ~> ^(parseExceptName ~ rep1sep(parseExceptType, line("|"))) ~
        (^(line("=>")) ~> ^(positioned(parseStatement)))) ^^ {
            case name ~ excepts ~ body => new NodeExcept(name, excepts, body)
        }

    private lazy val parseWhile         : Parser[NodeWhile]         =
        (line("while") ~> ^(positioned(parseExpr     ))) ~
        (line("do"   ) ~> ^(positioned(parseStatement))) ^^ {
            case expr ~ body => new NodeWhile(expr, body)
        }

    private lazy val parseSwitch        : Parser[NodeSwitch]        =
        (line("switch") ~> ^(positioned(parseExpr)) <~ ^(line("of"))) ~
        (^(line("{")) ~> rep1(parseCaseItem) ~ (parseDefault ?) <~ ^(line("}"))) ^^ {
            case expr ~ (cases ~ default) => new NodeSwitch(expr, cases, default)
        }

    private lazy val parseDefault       : Parser[NodeCase]          =
        (line("default") ~ ^(line("=>"))) ~> ^(positioned(parseStatement) <~
        (^(guard(line("}"))) withErrorMessage "``default'' must be placed at the end of ``switch'' statement")) ^^ (new NodeCase(null, _))

    private lazy val parseCaseItem      : Parser[NodeCase]          = (guard(parseDefault) <~ failure("unexpected `default`")) |
        (line("case") ~> ^(positioned(parseExpr))) ~
        (^(line("=>")) ~> ^(positioned(parseStatement))) ^^ {
            case keys ~ body => new NodeCase(keys, body)
        }

    private lazy val parseFunctionDef   : Parser[NodeFunctionDef]   =
        (line("def") ~> ^(positioned(parseSetName))) ~
        (^(line("(")) ~> repsep(positioned(parseFormalArgs), line(",")) <~ ^(line(")"))) ~
        (^(line("=>")) ~> ^(positioned(parseStatement))) ^^ {
            case name ~ args ~ body => new NodeFunctionDef(name, args, body)
        }

    /* compiler part */

    private lazy val script = source + CharArrayReader.EofCh
    private lazy val program = (lexical.NewLine *) ~> (positioned(parseStatement) +) <~ lexical.EOF

    def parse = program(new lexical.Scanner(script)) match
    {
        case Success(result, _)     => result
        case NoSuccess(error, next) => throw new SyntaxError(error, next.pos.line, next.pos.column)
    }
}
