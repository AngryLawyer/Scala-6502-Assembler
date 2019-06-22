package scala_6502_assembler.error

trait AssemblerCompilationError
case class AssemblerLexerError(msg: String) extends AssemblerCompilationError
