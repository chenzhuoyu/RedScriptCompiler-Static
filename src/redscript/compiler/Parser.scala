package redscript.compiler

import redscript.compiler.ast._

import scala.language.{implicitConversions, postfixOps}
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader

class Parser(val source: String) extends StdTokenParsers with PackratParsers
{
    override type Tokens  = TokenSpace
    override val  lexical = new Tokenizer

    private def line(op: String): Parser[String] = (lexical.NewLine *) ~> op <~ (lexical.NewLine *)
    private def tail(op: String): Parser[String] = not(lexical.NewLine) ~> op <~ (lexical.NewLine *)

    private implicit def expr2node(v: NodeExpr): Either[NodeExpr, NodeValue] = Left(v)
    private implicit def value2node(v: NodeValue): Either[NodeExpr, NodeValue] = Right(v)

    /* literals */

    private lazy val parseName        = positioned(ident ^^ (new NodeName(_)))
    private lazy val parseIntConst    = positioned(accept("int"   , { case lexical.IntLit(value)    => new NodeIntConst(value)    }))
    private lazy val parseFloatConst  = positioned(accept("float" , { case lexical.FloatLit(value)  => new NodeFloatConst(value)  }))
    private lazy val parseStringConst = positioned(accept("string", { case lexical.StringLit(value) => new NodeStringConst(value) }))

    /* expressions */

    private lazy val parseExpr          : Parser[NodeExpr]          = positioned(parsePair    ) * (line("->") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "->", y)})
    private lazy val parsePair          : Parser[NodeExpr]          = positioned(parseBoolOr  ) * (line("in") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "in", y)})
    private lazy val parseBoolOr        : Parser[NodeExpr]          = positioned(parseBoolAnd ) * (line("||") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "||", y)})
    private lazy val parseBoolAnd       : Parser[NodeExpr]          = positioned(parseBitOr   ) * (line("&&") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "&&", y)})
    private lazy val parseBitOr         : Parser[NodeExpr]          = positioned(parseBitXor  ) * (line("|" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "|" , y)})
    private lazy val parseBitXor        : Parser[NodeExpr]          = positioned(parseBitAnd  ) * (line("^" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "^" , y)})
    private lazy val parseBitAnd        : Parser[NodeExpr]          = positioned(parseEquals  ) * (line("&" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "&" , y)})
    private lazy val parseEquals        : Parser[NodeExpr]          = positioned(parseCompares) * (line("!=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "!=", y)}
                                                                                                |  line("==") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "==", y)})
    private lazy val parseCompares      : Parser[NodeExpr]          = positioned(parseShifts  ) * (line("<" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<" , y)}
                                                                                                |  line(">" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">" , y)}
                                                                                                |  line("<=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<=", y)}
                                                                                                |  line(">=") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">=", y)})
    private lazy val parseShifts        : Parser[NodeExpr]          = positioned(parseAddSub  ) * (line("<<") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "<<", y)}
                                                                                                |  line(">>") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, ">>", y)})
    private lazy val parseAddSub        : Parser[NodeExpr]          = positioned(parseTerm    ) * (line("+" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "+" , y)}
                                                                                                |  line("-" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "-" , y)})
    private lazy val parseTerm          : Parser[NodeExpr]          = positioned(parsePower   ) * (line("*" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "*" , y)}
                                                                                                |  line("/" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "/" , y)}
                                                                                                |  line("%" ) ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "%" , y)})
    private lazy val parsePower         : Parser[NodeExpr]          = positioned(parseFactor  ) * (line("**") ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(x, "**", y)})

    /* values */

    private lazy val parseFactor        : Parser[NodeExpr]          =
        ( line("+") ~> positioned(parseFactor) ^^ (new NodeExpr(null, "+", _))
        | line("-") ~> positioned(parseFactor) ^^ (new NodeExpr(null, "-", _))
        | line("~") ~> positioned(parseFactor) ^^ (new NodeExpr(null, "~", _))
        | line("!") ~> positioned(parseFactor) ^^ (new NodeExpr(null, "!", _))
        |              positioned(parseRValue) ^^ (new NodeExpr(_, null, null)))

    private lazy val parseRValue        : Parser[NodeValue]         =
        ( positioned(parseIntConst)
        | positioned(parseFloatConst)
        | positioned(parseStringConst)
        | positioned(parseMap)
        | positioned(parseArray)
        | positioned(parseName)
        | line("(") ~> positioned(parseExpr) <~ line(")")
        ) ~ (
        ( positioned(parseInvokeAttr)
        | positioned(parseInvokeStack)
        | positioned(parseGetAttr)
        | positioned(parseGetIndex)) *
        ) ^^ { case value ~ modifiers => new NodeValue(value, modifiers) }

    private lazy val parseMap           : Parser[NodeMap]           = line("{") ~> repsep(positioned(parseMapItem), line(",")) <~ line("}")    ^^ (new NodeMap(_))
    private lazy val parseMapItem       : Parser[NodeMapItem]       = positioned(parseExpr) ~ (tail(":") ~> positioned(parseExpr))             ^^ { case key ~ value => new NodeMapItem(key, value) }
    private lazy val parseInvokeAttr    : Parser[NodeInvokeAttr]    = parseGetAttr ~ parseInvokeStack                                          ^^ { case attr ~ invoke => new NodeInvokeAttr(attr, invoke) }

    private lazy val parseArray         : Parser[NodeArray]         = line("[") ~> repsep(positioned(parseExpr), line(",")) <~ line("]")       ^^ (new NodeArray(_))
    private lazy val parseInvokeStack   : Parser[NodeInvokeStack]   = tail("(") ~> repsep(positioned(parseActualArgs), line(",")) <~ line(")") ^^ (new NodeInvokeStack(_))

    private lazy val parseGetAttr       : Parser[NodeAttr]          = tail(".") ~> positioned(parseName)                                       ^^ (new NodeAttr(_, NodeAttr.Get))
    private lazy val parseGetIndex      : Parser[NodeIndex]         = tail("[") ~> positioned(parseExpr) <~ line("]")                          ^^ (new NodeIndex(_, isGet = true))

    private lazy val parseExpand        : Parser[Boolean]           = (line(">") ?) ^^ (_.isDefined)
    private lazy val parseActualArgs    : Parser[NodeArgument]      = parseExpand ~ positioned(parseExpr) ^^ { case expand ~ expr => new NodeArgument(expr, expand) }

    /* statements */

    private lazy val parseCompond       : Parser[NodeCompond]       = line("{") ~> (positioned(parseStatement) *) <~ line("}") ^^ (new NodeCompond(_))
    private lazy val parseStatement     : Parser[NodeStatement]     =
        ( positioned(parseExpr   ) ^^ (new NodeStatement(_))
        | positioned(parseRValue ) ^^ (new NodeStatement(_))
        | positioned(parseCompond) ^^ (new NodeStatement(_))
        ) <~ (";" | guard(lexical.EOF) | (lexical.NewLine +))

    /* compiler part */

    private lazy val script = source + CharArrayReader.EofCh
    private lazy val program = (lexical.NewLine *) ~> (positioned(parseStatement) *) <~ lexical.EOF

    def parse: List[NodeStatement] = program(new lexical.Scanner(script)) match
    {
        case Success(result, _)       => result
        case NoSuccess(message, next) => throw new SyntaxError(message, next.pos.line, next.pos.column)
    }
}
