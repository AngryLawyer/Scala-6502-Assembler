import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.lexer.{
  COMMENT,
  NUMBER,
  INSTRUCTION,
  LABEL,
  DIRECTIVE,
  NEWLINE,
  HASH,
}
import scala_6502_assembler.parser.{
  AssemblerParser,
  Line,
  LDA,
  Immediate,
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
          NEWLINE
        )
      )
    )
    assert { result.successful }
    assert {
      result.get == Line(LDA(Immediate(2)))
    }
  }
}
