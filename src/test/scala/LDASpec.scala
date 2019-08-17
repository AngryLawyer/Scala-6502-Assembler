import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.lexer._
import scala_6502_assembler.parser._

class LDASpec extends FlatSpec with DiagrammedAssertions {

  it should "Parse a zeroPage" in {
    val result = AssemblerParser.instructionLine(
      new AssemblerParser.AssemblerTokenReader(
        List(
          STRING("LDA"),
          BYTE(2),
          COMMENT("; Load 2 into accumulator"),
          NEWLINE()
        )
      )
    )
    assert { result.successful }
    assert {
      result.get == InstructionLine(None, LDA(ZeroPage(AddressingModeNumber(2))), None)
    }
  }

  it should "Parse a zeroPageX" in {
    val result = AssemblerParser.instructionLine(
      new AssemblerParser.AssemblerTokenReader(
        List(
          STRING("LDA"),
          BYTE(2),
          COMMA(),
          STRING("X"),
          NEWLINE()
        )
      )
    )
    assert { result.successful }
    assert {
      result.get == InstructionLine(None, LDA(ZeroPageX(AddressingModeNumber(2))), None)
    }
  }
}
