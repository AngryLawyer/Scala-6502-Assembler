package scala_6502_assembler

import scala.util.parsing.combinator._

sealed trait AssemblerToken

case class COMMENT(text: String) extends AssemblerToken
case class NUMBER(value: Int) extends AssemblerToken
case class INSTRUCTION(text: String) extends AssemblerToken
case class LABEL(text: String) extends AssemblerToken
case class DIRECTIVE(text: String) extends AssemblerToken

class AssemblerLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def comment: Parser[COMMENT] = ";[ -~]+".r ^^ { str => COMMENT(str) }
  def number: Parser[NUMBER] = "[0-9]+".r ^^ { str => NUMBER(str.toInt) }
  def decimal: Parser[NUMBER] = "#[0-9]+".r ^^ { str => NUMBER(str.substring(1).toInt) }
  def hexadecimal: Parser[NUMBER] = """\$[0-9A-F]+""".r ^^ { str => NUMBER(Integer.parseInt(str.substring(1), 16)) }
  def instruction: Parser[INSTRUCTION] = "[A-Z]{3}".r ^^ { str => INSTRUCTION(str) }
  def label: Parser[LABEL] = "[A-Za-z]+".r ^^ { str => LABEL(str) }
  def directive: Parser[DIRECTIVE] = """\.[A-Z]+""".r ^^ { str => DIRECTIVE(str) }
}
