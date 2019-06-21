import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.{
  AssemblerLexer,
  AssemblerParser,
  COMMENT,
  NUMBER,
  INSTRUCTION,
  LABEL,
  DIRECTIVE,
  NEWLINE,
  HASH
}

class LexerSpec extends FlatSpec with DiagrammedAssertions {
  behavior of "Comment Tokenizing"

  it should "tokenize comments" in {
    val result =
      AssemblerLexer.parse(AssemblerLexer.comment, "; Here is a comment")
    assert { result.successful }
    val internal = result.get match {
      case COMMENT(string) => Some(string)
      case _               => None
    }
    assert { internal.get == "; Here is a comment" }
  }

  behavior of "Hash Tokenizing"

  it should "tokenize hashes" in {
    val result = AssemblerLexer.parse(AssemblerLexer.hash, "#")
    assert { result.successful }
    val internal = result.get match {
      case HASH => true
      case _    => false
    }
    assert { internal == true }
  }

  behavior of "Number Tokenizing"

  it should "tokenize decimals" in {
    val result = AssemblerLexer.parse(AssemblerLexer.decimal, "12345")
    assert { result.successful }
    val internal = result.get match {
      case NUMBER(n) => Some(n)
      case _         => None
    }
    assert { internal.get == 12345 }
  }

  it should "tokenize hex" in {
    val result = AssemblerLexer.parse(AssemblerLexer.hexadecimal, "$DEAD")
    assert { result.successful }
    val internal = result.get match {
      case NUMBER(n) => Some(n)
      case _         => None
    }
    assert { internal.get == 0xDEAD }
  }

  behavior of "Instruction tokenizing"

  it should "Tokenize instructions" in {
    val result = AssemblerLexer.parse(AssemblerLexer.instruction, "POP")
    assert { result.successful }
    val internal = result.get match {
      case INSTRUCTION(n) => Some(n)
      case _              => None
    }
    assert { internal.get == "POP" }
  }

  behavior of "Label tokenizing"

  it should "Tokenize labels" in {
    val result = AssemblerLexer.parse(AssemblerLexer.label, "Shumliduc")
    assert { result.successful }
    val internal = result.get match {
      case LABEL(n) => Some(n)
      case _        => None
    }
    assert { internal.get == "Shumliduc" }
  }

  behavior of "Directive tokenizing"

  it should "Tokenize directives" in {
    val result = AssemblerLexer.parse(AssemblerLexer.directive, ".BYTE")
    assert { result.successful }
    val internal = result.get match {
      case DIRECTIVE(n) => Some(n)
      case _            => None
    }
    assert { internal.get == ".BYTE" }
  }

  behavior of "Newline tokenizing"

  it should "Tokenize newlines" in {
    val result = AssemblerLexer.parse(AssemblerLexer.newline, "\n")
    assert { result.successful }
    val internal = result.get match {
      case NEWLINE => true
      case _       => false
    }
    assert { internal == true }
  }

  behavior of "Full tokenizing"

  it should "Tokenize arbitrary programs" in {
    val result = AssemblerLexer("""LDA #02 ; Load 2 into accumulator
    ADC #02 ; Add 2 to accumulator
    STA $CB ; Store accumulator in 0xCB
    """)
    assert { result.isRight }
    assert {
      result.right.get ==
        List(
          INSTRUCTION("LDA"),
          HASH,
          NUMBER(2),
          COMMENT("; Load 2 into accumulator"),
          NEWLINE,
          INSTRUCTION("ADC"),
          HASH,
          NUMBER(2),
          COMMENT("; Add 2 to accumulator"),
          NEWLINE,
          INSTRUCTION("STA"),
          NUMBER(203),
          COMMENT("; Store accumulator in 0xCB"),
          NEWLINE
        )
    }
  }

  it should "Add a newline if one is missing" in {
    val result = AssemblerLexer("LDA #02 ; Load 2 into accumulator")
    assert { result.isRight }
    assert {
      result.right.get == List(
        INSTRUCTION("LDA"),
        HASH,
        NUMBER(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE
      )
    }
  }

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
  }
}
