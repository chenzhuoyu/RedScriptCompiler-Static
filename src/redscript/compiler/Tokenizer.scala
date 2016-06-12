package redscript.compiler

import scala.language.postfixOps
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader._

class Tokenizer extends StdLexical with TokenSpace
{
    reserved.clear()
    delimiters.clear()

    reserved ++= List(
        "do",
        "if",
        "in",
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
        "->" ,
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
    private def convertInt(x: String, radix: Int): Token = try IntLit(java.lang.Long.parseLong(x, radix)) catch { case _: NumberFormatException => LongLit(BigInt(x, radix)) }

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

    private lazy val identifier: Parser[Token] =
        identChar ~ ((identChar | digit) *) ^^ { case first ~ rest => processIdent(first :: rest mkString) }

    private lazy val numberLiteral: Parser[Token] =
        ( '.' ~> stringOf(digit)                                                        ^^ { case       fract => FloatLit(   s"0.$fract".toFloat) }
        | stringOf(digit) ~ ('.' ~> stringOf(digit))                                    ^^ { case int ~ fract => FloatLit(s"$int.$fract".toFloat) }
        | ('0' ~ (elem('b') | 'B')) ~> stringOf(binaryDigit) <~ (elem('l') | 'L')       ^^ { value => LongLit(BigInt(value, 2)) }
        | ('0' ~ (elem('x') | 'X')) ~> stringOf(hexadecimalDigit) <~ (elem('l') | 'L')  ^^ { value => LongLit(BigInt(value, 16)) }
        |  '0' ~> stringOf(octalDigit) <~ (elem('l') | 'L')                             ^^ { value => LongLit(BigInt(value)) }
        | stringOf(digit) <~ (elem('l') | 'L')                                          ^^ { value => LongLit(BigInt(value)) }
        | ('0' ~ (elem('b') | 'B')) ~> stringOf(binaryDigit)                            ^^ { value => convertInt(value,  2) }
        | ('0' ~ (elem('x') | 'X')) ~> stringOf(hexadecimalDigit)                       ^^ { value => convertInt(value, 16) }
        |  '0' ~> stringOf(octalDigit)                                                  ^^ { value => convertInt(value,  8) }
        | stringOf(digit)                                                               ^^ { value => convertInt(value, 10) })

    private lazy val stringLiteral: Parser[Token] =
        ( '\'' ~> '\''  ^^ { case _ => StringLit("") }
        | '\"' ~> '\"'  ^^ { case _ => StringLit("") }
        | '\'' ~> stringOf(escape | chrExcept('\\', '\'', EofCh)) <~ '\'' ^^ StringLit
        | '\"' ~> stringOf(escape | chrExcept('\\', '\"', EofCh)) <~ '\"' ^^ StringLit
        | '\'' ~> failure("Unclosed string literal")
        | '\"' ~> failure("Unclosed string literal"))

    override def token: Parser[Token] =
        ( delim
        | EofCh         ^^^ EOF
        | NewLine.EolCh ^^^ NewLine
        | whitespace ~> identifier    <~ whitespace
        | whitespace ~> numberLiteral <~ whitespace
        | whitespace ~> stringLiteral <~ whitespace
        | failure("Illegal character"))


    override def comment: Parser[Any] = '#' ~ (chrExcept(EofCh, NewLine.EolCh) *)
    override def whitespace: Parser[Any] = (comment | whitespaceChar | ('\\' ~ NewLine.EolCh)) *
    override def whitespaceChar: Parser[Char] = elem("whitespace chars", ch => ch != EofCh && ch != NewLine.EolCh && ch.isWhitespace)
}
