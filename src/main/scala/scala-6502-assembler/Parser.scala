package scala_6502_assembler

import scala.util.parsing.combinator._

case class Comment(text: String)

class AssemblerParser extends RegexParsers {
  def semicolon: Parser[String] = """;""".r ^^ { _.toString }
  def word: Parser[String] = """[A-Za-z]+""".r ^^ { _.toString }
  def printable: Parser[String] = """[ -~]+""".r ^^ { _.toString }
  def comment: Parser[Comment] = semicolon ~ printable ^^ { case _ ~ content => Comment(content) }
}
