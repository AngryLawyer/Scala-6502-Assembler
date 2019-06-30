package scala_6502_assembler.lexer
import scala.util.parsing.input.Positional

sealed trait AssemblerToken extends Positional

case class COMMENT(text: String) extends AssemblerToken
case class BYTE(value: Integer) extends AssemblerToken
case class TWOBYTES(value: Integer) extends AssemblerToken
case class INSTRUCTION(text: String) extends AssemblerToken
case class LABEL(text: String) extends AssemblerToken
case class DIRECTIVE(text: String) extends AssemblerToken
case class NEWLINE() extends AssemblerToken
case class HASH() extends AssemblerToken
case class ASTERISK() extends AssemblerToken
case class EQUALS() extends AssemblerToken
