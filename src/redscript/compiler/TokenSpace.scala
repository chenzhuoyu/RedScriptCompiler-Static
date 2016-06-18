package redscript.compiler

import scala.util.parsing.combinator.token.StdTokens

trait TokenSpace extends StdTokens
{
    case class IntLit(value: Long) extends Token
    {
        override def chars: String = value.toString
        override def toString: String = chars
    }

    case class LongLit(value: BigInt) extends Token
    {
        override def chars: String = value.toString
        override def toString: String = chars
    }

    case class FloatLit(value: Double) extends Token
    {
        override def chars: String = value.toString
        override def toString: String = chars
    }

    case object NewLine extends Token
    {
        override def chars: String = "<NewLine>"
    }
}
