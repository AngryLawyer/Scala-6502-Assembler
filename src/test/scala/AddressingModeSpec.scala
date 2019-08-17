import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.lexer._
import scala_6502_assembler.parser._

class AddressingModeSpec extends FlatSpec with DiagrammedAssertions {

  behavior of "Parsing Addressing Mode"

  it should "Parse Immediate mode" in {

    val result = AssemblerParser.immediate(
      new AssemblerParser.AssemblerTokenReader(
        List(
          HASH(),
          BYTE(2)
        )
      )
    )
    assert { result.successful }
    assert {
      result.get == Immediate(AddressingModeNumber(2))
    }
  }

  it should "Parse Zero Page X" in {

    val result = AssemblerParser.zeroPageX(
      new AssemblerParser.AssemblerTokenReader(
        List(
          BYTE(2),
          COMMA(),
          STRING("X"),
        )
      )
    )
    assert { result.successful }
    assert {
      result.get == ZeroPageX(AddressingModeNumber(2))
    }
  }
}
