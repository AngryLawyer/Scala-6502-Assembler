import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.lexer._
import scala_6502_assembler.parser._

class ParserSpec extends FlatSpec with DiagrammedAssertions {

  behavior of "Parsing"

  it should "Parse a .END directive" in {
    val result = AssemblerParser.end(
      new AssemblerParser.AssemblerTokenReader(
        List(
          DIRECTIVE(".END"),
          NEWLINE(),
          STRING("ADC"),
          HASH(),
          BYTE(2),
          NEWLINE()
        )
      )
    )
    assert { result.successful }
    assert {
      result.get == End()
    }
    assert {
      result.next.atEnd
    }
  }

  it should "Stop parsing after a .END directive" in {
    val result = AssemblerParser(
      List(
        STRING("LDA"),
        HASH(),
        BYTE(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE(),
        DIRECTIVE(".END"),
        NEWLINE(),
        STRING("ADC"),
        HASH(),
        BYTE(2),
        COMMENT("; Add 2 to accumulator"),
        NEWLINE()
      )
    )
    assert { result.isRight }
    assert {
      result.right.get == (
        Section(
          0,
          InstructionLine(
            None,
            LDA(Immediate(AddressingModeNumber(2))),
            None
          ),
          None
        )
      )
    }
  }

  it should "Parse a simple program" in {
    val result = AssemblerParser(
      List(
        STRING("LDA"),
        HASH(),
        BYTE(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE(),
        STRING("ADC"),
        HASH(),
        BYTE(2),
        COMMENT("; Add 2 to accumulator"),
        NEWLINE(),
        STRING("STA"),
        BYTE(203),
        COMMENT("; Store accumulator in 0xCB"),
        NEWLINE()
      )
    )
    assert { result.isRight }
    assert {
      result.right.get == (
        Section(
          0,
          InstructionLine(
            None,
            LDA(Immediate(AddressingModeNumber(2))),
            Some(
              InstructionLine(
                None,
                ADC(Immediate(AddressingModeNumber(2))),
                Some(
                  InstructionLine(
                    None,
                    STA(ZeroPage(AddressingModeNumber(0xCB))),
                    None
                  )
                )
              )
            )
          ),
          None
        )
      )
    }
  }

  it should "Handle comments and newlines in code" in {
    val result = AssemblerParser(
      List(
        STRING("LDA"),
        HASH(),
        BYTE(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE(),
        NEWLINE(),
        STRING("ADC"),
        HASH(),
        BYTE(2),
        COMMENT("; Add 2 to accumulator"),
        NEWLINE(),
        COMMENT("; And then the last bit"),
        NEWLINE(),
        STRING("STA"),
        BYTE(203),
        COMMENT("; Store accumulator in 0xCB"),
        NEWLINE()
      )
    )
    assert { result.isRight }
    assert {
      result.right.get == (
        Section(
          0,
          InstructionLine(
            None,
            LDA(Immediate(AddressingModeNumber(2))),
            Some(
              CommentedLine(
                None,
                Some(
                  InstructionLine(
                    None,
                    ADC(Immediate(AddressingModeNumber(2))),
                    Some(
                      CommentedLine(
                        None,
                        Some(InstructionLine(None, STA(ZeroPage(AddressingModeNumber(203))), None))
                      )
                    )
                  )
                )
              )
            )
          ),
          None
        )
      )
    }
  }
}
