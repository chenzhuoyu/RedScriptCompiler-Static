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
        "super",
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
        "::" , ":"  , "@" , ";"
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
        ( octalDigit ~ octalDigit ~ octalDigit       ^^ { case a ~ b ~ c => (a * 64 + b * 8 + c).toChar }
        | octalDigit ~ octalDigit                    ^^ { case a ~ b     => (a *  8 + b        ).toChar }
        | octalDigit                                 ^^ { case a         =>  a                  .toChar }
        | 'x' ~> hexadecimalDigit ~ hexadecimalDigit ^^ { case a ~ b     => (a * 16 + b        ).toChar }
        | 'X' ~> hexadecimalDigit ~ hexadecimalDigit ^^ { case a ~ b     => (a * 16 + b        ).toChar }
        | '\\'
        | '\"'
        | '\''
        | 'b' ^^^ '\b'
        | 'f' ^^^ '\f'
        | 'n' ^^^ '\n'
        | 'r' ^^^ '\r'
        | 't' ^^^ '\t')

    private lazy val identifier: Parser[Token] = """[\p{javaJavaIdentifierStart}\p{javaUnicodeIdentifierStart}][\p{javaJavaIdentifierPart}\p{javaUnicodeIdentifierPart}]*""".r ^^ processIdent
    private lazy val numberLiteral: Parser[Token] =
        ( stringOf(digit) ~ ('.' ~> stringOf(digit))                        ^^ { case int ~ fract => FloatLit(s"$int.$fract".toDouble) }
        | ('0' ~ 'b') ~> stringOf(binaryDigit     ) ~ opt(elem('l') | 'L')  ^^ { case value ~ None => convertInt(value,  2) case value ~ Some(_) => LongLit(new BigInteger(value,  2)) }
        | ('0' ~ 'B') ~> stringOf(binaryDigit     ) ~ opt(elem('l') | 'L')  ^^ { case value ~ None => convertInt(value,  2) case value ~ Some(_) => LongLit(new BigInteger(value,  2)) }
        | ('0' ~ 'x') ~> stringOf(hexadecimalDigit) ~ opt(elem('l') | 'L')  ^^ { case value ~ None => convertInt(value, 16) case value ~ Some(_) => LongLit(new BigInteger(value, 16)) }
        | ('0' ~ 'X') ~> stringOf(hexadecimalDigit) ~ opt(elem('l') | 'L')  ^^ { case value ~ None => convertInt(value, 16) case value ~ Some(_) => LongLit(new BigInteger(value, 16)) }
        |  '0' ~>        stringOf(octalDigit      ) ~ opt(elem('l') | 'L')  ^^ { case value ~ None => convertInt(value,  8) case value ~ Some(_) => LongLit(new BigInteger(value,  8)) }
        |                stringOf(digit           ) ~ opt(elem('l') | 'L')  ^^ { case value ~ None => convertInt(value, 10) case value ~ Some(_) => LongLit(new BigInteger(value, 10)) })

    private lazy val stringLiteral: Parser[Token] =
        ( '\'' ~> '\''  ^^ { case _ => StringLit("") }
        | '\"' ~> '\"'  ^^ { case _ => StringLit("") }
        | '\'' ~> stringOf(escape | chrExcept('\\', '\'', EofCh)) <~ '\'' ^^ StringLit
        | '\"' ~> stringOf(escape | chrExcept('\\', '\"', EofCh)) <~ '\"' ^^ StringLit
        | '\'' ~> failure("Unclosed string literal")
        | '\"' ~> failure("Unclosed string literal"))

    override def token: Parser[Token] =
        ( EofCh     ^^^ EOF
        | '\f'      ^^^ NewLine
        | '\r'      ^^^ NewLine
        | '\n'      ^^^ NewLine
        | '\u0085'  ^^^ NewLine
        | '\u2028'  ^^^ NewLine
        | '\u2029'  ^^^ NewLine
        | whitespace ~> identifier    <~ whitespace
        | whitespace ~> numberLiteral <~ whitespace
        | whitespace ~> stringLiteral <~ whitespace
        | delim
        | failure("Illegal character"))

    override def comment: Parser[Any] = '#' ~ (chrExcept(EofCh, '\f', '\r', '\n', '\u0085', '\u2028', '\u2029') *)
    override def whitespace: Parser[Any] = (comment | whitespaceChar | ('\\' ~ (elem('\f') | '\r' | '\n' | '\u0085' | '\u2028' | '\u2029'))) *
    override def whitespaceChar: Parser[Char] = elem("whitespace characters", ch => ch != EofCh && ch.isWhitespace && !"\f\r\n\u0085\u2028\u2029".contains(ch))

    override protected def processIdent(identifier: String) = identifier match
    {
        case "null"  => NullLit
        case "true"  => BooleanLit(true)
        case "false" => BooleanLit(false)
        case _       => super.processIdent(Normalizer.normalize(identifier, Normalizer.Form.NFC))
    }

    java.lang.Character.isUnicodeIdentifierStart(123)
}
