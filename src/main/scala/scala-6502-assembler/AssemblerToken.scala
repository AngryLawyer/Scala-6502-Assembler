package scala_6502_assembler.lexer
sealed trait AssemblerToken

case class COMMENT(text: String) extends AssemblerToken
case class NUMBER(value: Int) extends AssemblerToken
case class INSTRUCTION(text: String) extends AssemblerToken
case class LABEL(text: String) extends AssemblerToken
case class DIRECTIVE(text: String) extends AssemblerToken
case object NEWLINE extends AssemblerToken
case object HASH extends AssemblerToken
