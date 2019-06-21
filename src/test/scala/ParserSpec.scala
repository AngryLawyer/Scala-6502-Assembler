import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.{
  AssemblerLexer,
  COMMENT,
  NUMBER,
  INSTRUCTION,
  LABEL,
  DIRECTIVE,
  NEWLINE
}

class LexerSpec extends FlatSpec with DiagrammedAssertions {
  behavior of "Comment Tokenizing"

  it should "tokenize comments" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.comment, "; Here is a comment")
    assert { result.successful }
    val internal = result.get match {
      case COMMENT(string) => Some(string)
      case _               => None
    }
    assert { internal.get == "; Here is a comment" }
  }

  behavior of "Number Tokenizing"

  it should "tokenize plain numbers" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.number, "12345")
    assert { result.successful }
    val internal = result.get match {
      case NUMBER(n) => Some(n)
      case _         => None
    }
    assert { internal.get == 12345 }
  }

  it should "tokenize decimals" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.decimal, "#12345")
    assert { result.successful }
    val internal = result.get match {
      case NUMBER(n) => Some(n)
      case _         => None
    }
    assert { internal.get == 12345 }
  }

  it should "tokenize hex" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.hexadecimal, "$DEAD")
    assert { result.successful }
    val internal = result.get match {
      case NUMBER(n) => Some(n)
      case _         => None
    }
    assert { internal.get == 0xDEAD }
  }

  behavior of "Instruction tokenizing"

  it should "Tokenize instructions" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.instruction, "POP")
    assert { result.successful }
    val internal = result.get match {
      case INSTRUCTION(n) => Some(n)
      case _              => None
    }
    assert { internal.get == "POP" }
  }

  behavior of "Label tokenizing"

  it should "Tokenize labels" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.label, "Shumliduc")
    assert { result.successful }
    val internal = result.get match {
      case LABEL(n) => Some(n)
      case _        => None
    }
    assert { internal.get == "Shumliduc" }
  }

  behavior of "Directive tokenizing"

  it should "Tokenize directives" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.directive, ".BYTE")
    assert { result.successful }
    val internal = result.get match {
      case DIRECTIVE(n) => Some(n)
      case _            => None
    }
    assert { internal.get == ".BYTE" }
  }

  behavior of "Newline tokenizing"

  it should "Tokenize newlines" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.newline, "\n")
    assert { result.successful }
    val internal = result.get match {
      case NEWLINE => true
      case _       => false
    }
    assert { internal == true }
  }

  behavior of "Full tokenizing"

  it should "Tokenize arbitrary programs" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.tokens, """LDA #02 ; Load 2 into accumulator
    ADC #02 ; Add 2 to accumulator
    STA $CB ; Store accumulator in 0xCB
    """)
    assert { result.successful }
    assert {
      result.get == List(
        INSTRUCTION("LDA"),
        NUMBER(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE,
        INSTRUCTION("ADC"),
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
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.tokens, "LDA #02 ; Load 2 into accumulator")
    assert { result.successful }
    assert {
      result.get == List(
        INSTRUCTION("LDA"),
        NUMBER(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE
      )
    }
  }
}
