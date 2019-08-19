package scala_6502_assembler.lexer
import scala.util.parsing.input.Positional

sealed trait AssemblerToken extends Positional

case class COMMENT(text: String) extends AssemblerToken
case class BYTE(value: Int) extends AssemblerToken
case class TWOBYTES(value: Int) extends AssemblerToken
case class STRING(text: String) extends AssemblerToken
case class DIRECTIVE(text: String) extends AssemblerToken
case class NEWLINE() extends AssemblerToken
case class HASH() extends AssemblerToken
case class ASTERISK() extends AssemblerToken
case class EQUALS() extends AssemblerToken
case class COMMA() extends AssemblerToken
case class QUOTE(text: String) extends AssemblerToken
