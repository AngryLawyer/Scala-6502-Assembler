import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.lexer.{
  COMMENT,
  NUMBER,
  INSTRUCTION,
  LABEL,
  DIRECTIVE,
  NEWLINE,
  HASH
}
import scala_6502_assembler.parser.{
  AssemblerParser,
  Line,
  LDA,
  ADC,
  STA,
  Immediate,
  ZeroPage
}

class ParserSpec extends FlatSpec with DiagrammedAssertions {

  behavior of "Parsing"

  it should "Parse a simple line" in {
    val result = AssemblerParser.line(
      new AssemblerParser.AssemblerTokenReader(
        List(
          INSTRUCTION("LDA"),
          NUMBER(2),
          COMMENT("; Load 2 into accumulator"),
          NEWLINE()
        )
      )
    )
    assert { result.successful }
    assert {
      result.get == Line(LDA(ZeroPage(2)))
    }
  }

  it should "Parse a simple program" in {
    val result = AssemblerParser(
      List(
        INSTRUCTION("LDA"),
        HASH(),
        NUMBER(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE(),
        INSTRUCTION("ADC"),
        HASH(),
        NUMBER(2),
        COMMENT("; Add 2 to accumulator"),
        NEWLINE(),
        INSTRUCTION("STA"),
        NUMBER(203),
        COMMENT("; Store accumulator in 0xCB"),
        NEWLINE()
      )
    )
    assert { result.isRight }
    assert {
      result.right.get == List(
        Line(LDA(Immediate(2))),
        Line(ADC(Immediate(2))),
        Line(STA(ZeroPage(0xCB)))
      )
    }
  }
}
