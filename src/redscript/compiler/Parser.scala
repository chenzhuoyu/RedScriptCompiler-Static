package redscript.compiler

import redscript.compiler.ast._

import scala.language.{implicitConversions, postfixOps}
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader

class Parser(val source: String) extends StdTokenParsers
{
    override type Tokens  = TokenSpace
    override val  lexical = new Tokenizer

    private def ^[T](p: Parser[T]): Parser[T] = commit(p)
    private def line[T](p: Parser[T]): Parser[T] = (lexical.NewLine *) ~> p <~ guard(lexical.NewLine *)
    private def tail[T](p: Parser[T]): Parser[T] = not(lexical.NewLine) ~> p <~ guard(lexical.NewLine *)

    private implicit def expr2node(v: NodeExpr): Either[NodeExpr, NodeValue] = Left(v)
    private implicit def value2node(v: NodeValue): Either[NodeExpr, NodeValue] = Right(v)

    /* literals */

    private lazy val parseIntConst      : Parser[NodeIntConst]      = positioned(accept("int"   , { case lexical.IntLit(value)    => new NodeIntConst(value)    }))
    private lazy val parseFloatConst    : Parser[NodeFloatConst]    = positioned(accept("float" , { case lexical.FloatLit(value)  => new NodeFloatConst(value)  }))
    private lazy val parseStringConst   : Parser[NodeStringConst]   = positioned(accept("string", { case lexical.StringLit(value) => new NodeStringConst(value) }))

    /* expressions */

    private lazy val parseExpr          : Parser[NodeExpr]          = positioned(parsePair    ) * (tail("->") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "->", y)})
    private lazy val parsePair          : Parser[NodeExpr]          = positioned(parseBoolOr  ) * (tail("in") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "in", y)})
    private lazy val parseBoolOr        : Parser[NodeExpr]          = positioned(parseBoolAnd ) * (tail("||") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "||", y)})
    private lazy val parseBoolAnd       : Parser[NodeExpr]          = positioned(parseBitOr   ) * (tail("&&") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "&&", y)})
    private lazy val parseBitOr         : Parser[NodeExpr]          = positioned(parseBitXor  ) * (tail("|" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "|" , y)})
    private lazy val parseBitXor        : Parser[NodeExpr]          = positioned(parseBitAnd  ) * (tail("^" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "^" , y)})
    private lazy val parseBitAnd        : Parser[NodeExpr]          = positioned(parseEquals  ) * (tail("&" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "&" , y)})
    private lazy val parseEquals        : Parser[NodeExpr]          = positioned(parseCompares) * (tail("!=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "!=", y)}
                                                                                                |  tail("==") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "==", y)})
    private lazy val parseCompares      : Parser[NodeExpr]          = positioned(parseShifts  ) * (tail("<" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<" , y)}
                                                                                                |  tail(">" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">" , y)}
                                                                                                |  tail("<=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<=", y)}
                                                                                                |  tail(">=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">=", y)})
    private lazy val parseShifts        : Parser[NodeExpr]          = positioned(parseAddSub  ) * (tail("<<") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<<", y)}
                                                                                                |  tail(">>") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">>", y)})
    private lazy val parseAddSub        : Parser[NodeExpr]          = positioned(parseTerm    ) * (tail("+" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "+" , y)}
                                                                                                |  tail("-" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "-" , y)})
    private lazy val parseTerm          : Parser[NodeExpr]          = positioned(parsePower   ) * (tail("*" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "*" , y)}
                                                                                                |  tail("/" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "/" , y)}
                                                                                                |  tail("%" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "%" , y)})
    private lazy val parsePower         : Parser[NodeExpr]          = positioned(parseFactor  ) * (tail("**") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "**", y)})

    /* values */

    private lazy val parseFactor        : Parser[NodeExpr]          =
        ( "+" ~> ^(positioned(parseFactor)) ^^ (new NodeExpr(null, "+", _))
        | "-" ~> ^(positioned(parseFactor)) ^^ (new NodeExpr(null, "-", _))
        | "~" ~> ^(positioned(parseFactor)) ^^ (new NodeExpr(null, "~", _))
        | "!" ~> ^(positioned(parseFactor)) ^^ (new NodeExpr(null, "!", _))
        |           positioned(parseRValue)  ^^ (new NodeExpr(_, null, null)))

    private lazy val parseModOp         : Parser[String]            = guard(tail(".") | tail("[") | tail("("))
    private lazy val parseRValue        : Parser[NodeValue]         = positioned(parseBaseValue) ~ parseRModifiers ^^ { case value ~ rmods => new NodeValue(value, rmods, isRValue = true ) }
    private lazy val parseLValue        : Parser[NodeValue]         =
        ( parseSetName <~ not(parseModOp)               ^^ (new NodeValue(_, Nil, isRValue = false))
        | positioned(parseBaseValue) ~ parseLModifiers  ^^ { case value ~ lmods => new NodeValue(value, lmods, isRValue = false) })

    private lazy val parseBaseValue     : Parser[AST]               =
        ( positioned(parseIntConst      )
        | positioned(parseFloatConst    )
        | positioned(parseStringConst   )
        | positioned(parseMap           )
        | positioned(parseArray         )
        | positioned(parseGetName       )
        | tail("(") ~> ^(positioned(parseExpr)) <~ ^(line(")")))

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

    private lazy val parseMap           : Parser[NodeMap]           = line("{") ~> repsep(positioned(parseMapItem), line(",")) <~ ^(line("}"))    ^^ (new NodeMap(_))
    private lazy val parseArray         : Parser[NodeArray]         = line("[") ~> repsep(positioned(parseExpr), line(",")) <~ ^(line("]"))       ^^ (new NodeArray(_))
    private lazy val parseInvokeStack   : Parser[NodeInvokeStack]   = tail("(") ~> repsep(positioned(parseActualArgs), line(",")) <~ ^(line(")")) ^^ (new NodeInvokeStack(_))

    private lazy val parseMapItem       : Parser[NodeMapItem]       = positioned(parseExpr) ~ (^(tail(":")) ~> ^(positioned(parseExpr)))         ^^ { case key ~ value => new NodeMapItem(key, value) }
    private lazy val parseInvokeAttr    : Parser[NodeInvokeAttr]    = tail(".") ~> ^(positioned(parseGetName)) ~ ^(positioned(parseInvokeStack)) ^^ { case attr ~ invoke => new NodeInvokeAttr(attr, invoke) }

    private lazy val parseGetName       : Parser[NodeName]          = positioned(ident ^^ (new NodeName(_, isGet = true)))
    private lazy val parseSetName       : Parser[NodeName]          = positioned(ident ^^ (new NodeName(_, isGet = false)))

    private lazy val parseGetAttr       : Parser[NodeAttr]          = tail(".") ~> ^(positioned(parseGetName))                ^^ (new NodeAttr(_, isGet = true))
    private lazy val parseGetIndex      : Parser[NodeIndex]         = tail("[") ~> ^(positioned(parseExpr)) <~ ^(line("]"))   ^^ (new NodeIndex(_, isGet = true))

    private lazy val parseSetAttr       : Parser[NodeAttr]          = tail(".") ~> ^(positioned(parseGetName))                ^^ (new NodeAttr(_, isGet = false))
    private lazy val parseSetIndex      : Parser[NodeIndex]         = tail("[") ~> ^(positioned(parseExpr)) <~ ^(line("]"))   ^^ (new NodeIndex(_, isGet = false))

    private lazy val parseActualArgs    : Parser[NodeArgument]      =
      (                positioned(parseExpr)  ^^ (new NodeArgument(_, false))
      | line(">") ~> ^(positioned(parseExpr)) ^^ (new NodeArgument(_, true)))

    /* statements */

    private lazy val parseCompond       : Parser[NodeCompond]       = line("{") ~> (positioned(parseStatement) *) <~ ^(line("}")) ^^ (new NodeCompond(_))
    private lazy val parseStatement     : Parser[NodeStatement]     =
        ( positioned(parseIf        ) ^^ (new NodeStatement(_))
        | positioned(parseWhile     ) ^^ (new NodeStatement(_))
        | positioned(parseCompond   ) ^^ (new NodeStatement(_))
        | positioned(parseIncrement ) ^^ (new NodeStatement(_))
        | positioned(parseAssignment) ^^ (new NodeStatement(_))
        | positioned(parseExpr      ) ^^ (new NodeStatement(_))
        ) <~ (";" | guard(lexical.EOF) | (lexical.NewLine *))

    /* language structures */

    private lazy val parseIncOp         : Parser[String]            = tail("+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | ">>=")
    private lazy val parseIncrement     : Parser[NodeIncrement]     = positioned(parseLValue) ~ parseIncOp ~ ^(positioned(parseExpr)) ^^ { case target ~ op ~ expr => new NodeIncrement(target, op, expr) }
    private lazy val parseAssignment    : Parser[NodeAssignment]    = ((positioned(parseLValue) <~ tail("=")) +) ~ ^(positioned(parseExpr)) ^^ { case targets ~ expr => new NodeAssignment(targets, expr) }

    private lazy val parseIf            : Parser[NodeIf]            =
        ( (line("if"  )  ~> ^(positioned(parseExpr     ))) ~
        (^(line("then")) ~> ^(positioned(parseStatement))) ~
        ( (line("else")  ~> ^(positioned(parseStatement))) ?)) ^^ {
            case expr ~ success ~ failed => new NodeIf(expr, success, failed)
        }

    private lazy val parseWhile         : Parser[NodeWhile]         =
        (line("while") ~> ^(positioned(parseExpr     ))) ~
        (line("do"   ) ~> ^(positioned(parseStatement))) ^^ {
            case expr ~ body => new NodeWhile(expr, body)
        }

    /* compiler part */

    private lazy val script = source + CharArrayReader.EofCh
    private lazy val program = (lexical.NewLine *) ~> (positioned(parseStatement) +) <~ lexical.EOF

    def parse = program(new lexical.Scanner(script)) match
    {
        case Success(result, _)       => result
        case NoSuccess(message, next) => throw new SyntaxError(message, next.pos.line, next.pos.column)
    }
}
