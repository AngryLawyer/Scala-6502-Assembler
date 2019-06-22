package scala_6502_assembler.error

trait AssemblerCompilationError
case class AssemblerLexerError(location: Location, msg: String) extends AssemblerCompilationError
case class AssemblerParserError(location: Location, msg: String) extends AssemblerCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
