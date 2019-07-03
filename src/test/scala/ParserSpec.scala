import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.lexer.{
  COMMENT,
  BYTE,
  TWOBYTES,
  INSTRUCTION,
  LABEL,
  DIRECTIVE,
  NEWLINE,
  HASH,
  ASTERISK,
  EQUALS
}
import scala_6502_assembler.parser.{
  AssemblerParser,
  End,
  CommentedLine,
  InstructionLine,
  LDA,
  ADC,
  STA,
  Immediate,
  ZeroPage,
  Section
}

class ParserSpec extends FlatSpec with DiagrammedAssertions {

  behavior of "Parsing"

  it should "Parse a simple line" in {
    val result = AssemblerParser.instructionLine(
      new AssemblerParser.AssemblerTokenReader(
        List(
          INSTRUCTION("LDA"),
          BYTE(2),
          COMMENT("; Load 2 into accumulator"),
          NEWLINE()
        )
      )
    )
    assert { result.successful }
    assert {
      result.get == InstructionLine(LDA(ZeroPage(2)), None)
    }
  }

  it should "Parse a .END directive" in {
    val result = AssemblerParser.end(
      new AssemblerParser.AssemblerTokenReader(
        List(
          DIRECTIVE(".END"),
          NEWLINE(),
          INSTRUCTION("ADC"),
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
        INSTRUCTION("LDA"),
        HASH(),
        BYTE(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE(),
        DIRECTIVE(".END"),
        NEWLINE(),
        INSTRUCTION("ADC"),
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
            LDA(Immediate(2)),
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
        INSTRUCTION("LDA"),
        HASH(),
        BYTE(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE(),
        INSTRUCTION("ADC"),
        HASH(),
        BYTE(2),
        COMMENT("; Add 2 to accumulator"),
        NEWLINE(),
        INSTRUCTION("STA"),
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
            LDA(Immediate(2)),
            Some(
              InstructionLine(
                ADC(Immediate(2)),
                Some(
                  InstructionLine(
                    STA(ZeroPage(0xCB)),
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
        INSTRUCTION("LDA"),
        HASH(),
        BYTE(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE(),
        NEWLINE(),
        INSTRUCTION("ADC"),
        HASH(),
        BYTE(2),
        COMMENT("; Add 2 to accumulator"),
        NEWLINE(),
        COMMENT("; And then the last bit"),
        NEWLINE(),
        INSTRUCTION("STA"),
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
            LDA(Immediate(2)),
            Some(
              CommentedLine(
                Some(
                  InstructionLine(
                    ADC(Immediate(2)),
                    Some(
                      CommentedLine(
                        Some(InstructionLine(STA(ZeroPage(203)), None))
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
