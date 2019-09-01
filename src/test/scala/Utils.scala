package scala_6502_assembler.test
import scala_6502_assembler.lexer._
import scala_6502_assembler.parser._

object Utils {
  def tokenReader(tokens: List[AssemblerToken]) = {
      new AssemblerParser.AssemblerTokenReader(
        tokens
      )
  }
}
