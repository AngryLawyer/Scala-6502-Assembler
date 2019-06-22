package scala_6502_assembler.lexer
import scala.util.parsing.input.Positional

sealed trait AssemblerToken extends Positional

case class COMMENT(text: String) extends AssemblerToken
case class NUMBER(value: Int) extends AssemblerToken
case class INSTRUCTION(text: String) extends AssemblerToken
case class LABEL(text: String) extends AssemblerToken
case class DIRECTIVE(text: String) extends AssemblerToken
case class NEWLINE() extends AssemblerToken
case class HASH() extends AssemblerToken
