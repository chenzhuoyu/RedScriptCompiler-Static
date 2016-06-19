package redscript.compiler

import java.math.BigInteger
import java.text.Normalizer

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader._

class Tokenizer extends StdLexical with TokenSpace with RegexParsers
{
    override type Elem = Char

    reserved.clear()
    delimiters.clear()

    reserved ++= List(
        "do",
        "if",
        "in",
        "of",
        "def",
        "end",
        "for",
        "try",
        "case",
        "else",
        "then",
        "break",
        "class",
        "raise",
        "while",
        "delete",
        "except",
        "import",
        "return",
        "switch",
        "default",
        "finally",
        "continue"
    )

    delimiters ++= List(
        "("  , ")"  , "[" , "]" , "{" , "}" ,
        "->" , "=>" ,
        "==" , "!=" , "=" ,
        "&&" , "||" , "!" ,
        "&=" , "|=" , "^=",
        "&"  , "|"  , "^" , "~" ,
        "<<=", ">>=", "<=", ">=",
        "<<" , ">>" , "<" , ">" ,
        "**=", "+=" , "-=", "*=", "/=", "%=",
        "**" , "+"  , "-" , "*" , "/" , "%" ,
        ".." , "."  , "," ,
        ":"  , "@"  , "`" , "$" , ";"
    )

    private def stringOf(p: => Parser[Char]): Parser[String] = rep1(p) ^^ (_.mkString)
    private def convertInt(x: String, radix: Int): Token = try IntLit(Integer.parseInt(x, radix)) catch { case _: NumberFormatException => LongLit(new BigInteger(x, radix)) }

    private lazy val octalDigit      : Parser[Char] = elem("octal digit" , c => '0' <= c && c <= '7')
    private lazy val binaryDigit     : Parser[Char] = elem("binary digit", c => c == '0' || c == '1')
    private lazy val hexadecimalDigit: Parser[Char] = accept("hexadecimal digit", {
        case c @ ('a' | 'b' | 'c' | 'd' | 'e' | 'f')                         => c
        case c @ ('A' | 'B' | 'C' | 'D' | 'E' | 'F')                         => c
        case c @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => c
    })

    private lazy val escape: Parser[Char] = '\\' ~>
        ( octalDigit ~ octalDigit ~ octalDigit                     ^^ { case a ~ b ~ c => (a * 64 + b * 8 + c).toChar }
        | octalDigit ~ octalDigit                                  ^^ { case a ~ b     => (a *  8 + b        ).toChar }
        | octalDigit                                               ^^ { case a         =>  a                  .toChar }
        | (elem('x') | 'X') ~> hexadecimalDigit ~ hexadecimalDigit ^^ { case a ~ b     => (a * 16 + b        ).toChar }
        | '\\'
        | '\"'
        | '\''
        | 'b' ^^^ '\b'
        | 'f' ^^^ '\f'
        | 'n' ^^^ '\n'
        | 'r' ^^^ '\r'
        | 't' ^^^ '\t')

    private lazy val identifier: Parser[Token] = """[\p{javaUnicodeIdentifierStart}][\p{javaUnicodeIdentifierPart}']*""".r ^^ processIdent
    private lazy val numberLiteral: Parser[Token] =
        ( stringOf(digit) ~ ('.' ~> stringOf(digit))                                        ^^ { case int ~ fract => FloatLit(s"$int.$fract".toDouble) }
        | ('0' ~ (elem('b') | 'B')) ~> stringOf(binaryDigit     ) ~ opt(elem('l') | 'L')    ^^ { case value ~ suffix => if (suffix.isDefined) LongLit(new BigInteger(value,  2)) else convertInt(value,  2) }
        | ('0' ~ (elem('x') | 'X')) ~> stringOf(hexadecimalDigit) ~ opt(elem('l') | 'L')    ^^ { case value ~ suffix => if (suffix.isDefined) LongLit(new BigInteger(value, 16)) else convertInt(value, 16) }
        |  '0' ~>                      stringOf(octalDigit      ) ~ opt(elem('l') | 'L')    ^^ { case value ~ suffix => if (suffix.isDefined) LongLit(new BigInteger(value,  8)) else convertInt(value,  8) }
        |                              stringOf(digit           ) ~ opt(elem('l') | 'L')    ^^ { case value ~ suffix => if (suffix.isDefined) LongLit(new BigInteger(value, 10)) else convertInt(value, 10) })

    private lazy val stringLiteral: Parser[Token] =
        ( '\'' ~> '\''  ^^ { case _ => StringLit("") }
        | '\"' ~> '\"'  ^^ { case _ => StringLit("") }
        | '\'' ~> stringOf(escape | chrExcept('\\', '\'', EofCh)) <~ '\'' ^^ StringLit
        | '\"' ~> stringOf(escape | chrExcept('\\', '\"', EofCh)) <~ '\"' ^^ StringLit
        | '\'' ~> failure("Unclosed string literal")
        | '\"' ~> failure("Unclosed string literal"))

    override def token: Parser[Token] =
        ( whitespace ~> identifier    <~ whitespace
        | whitespace ~> numberLiteral <~ whitespace
        | whitespace ~> stringLiteral <~ whitespace
        | elem('\f')     ^^^ NewLine
        | elem('\r')     ^^^ NewLine
        | elem('\n')     ^^^ NewLine
        | elem('\u0085') ^^^ NewLine
        | elem('\u2028') ^^^ NewLine
        | elem('\u2029') ^^^ NewLine
        | EofCh          ^^^ EOF
        | delim
        | failure("Illegal character"))

    override def comment: Parser[Any] = '#' ~ (chrExcept(EofCh, '\f', '\r', '\n', '\u0085', '\u2028', '\u2029') *)
    override def whitespace: Parser[Any] = (comment | whitespaceChar | ('\\' ~ (elem('\f') | '\r' | '\n' | '\u0085' | '\u2028' | '\u2029'))) *
    override def whitespaceChar: Parser[Char] = elem("whitespace chars", ch => ch != EofCh && ch.isWhitespace && !"\f\r\n\u0085\u2028\u2029".contains(ch))

    override protected def processIdent(identifier: String) =
    {
        val form = Normalizer.Form.NFC
        super.processIdent(Normalizer.normalize(identifier, form))
    }
}
