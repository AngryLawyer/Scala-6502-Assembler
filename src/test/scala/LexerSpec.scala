import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.lexer.{
  AssemblerLexer,
  ASTERISK,
  EQUALS,
  COMMENT,
  BYTE,
  TWOBYTES,
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
    assert { result.get == COMMENT("; Here is a comment") }
  }

  behavior of "Hash Tokenizing"

  it should "tokenize hashes" in {
    val result = AssemblerLexer.parse(AssemblerLexer.hash, "#")
    assert { result.successful }
    assert { result.get == HASH() }
  }

  behavior of "Number Tokenizing"

  it should "tokenize byte decimals" in {
    val result = AssemblerLexer.parse(AssemblerLexer.decimal, "255")
    assert { result.successful }
    assert { result.get == BYTE(255) }
  }

  it should "tokenize byte hex" in {
    val result = AssemblerLexer.parse(AssemblerLexer.hexByte, "$DE")
    assert { result.successful }
    assert { result.get == BYTE(0xDE) }
  }

  it should "tokenize decimals" in {
    val result = AssemblerLexer.parse(AssemblerLexer.decimal, "12345")
    assert { result.successful }
    assert { result.get == TWOBYTES(12345) }
  }

  it should "tokenize hex" in {
    val result = AssemblerLexer.parse(AssemblerLexer.hexTwoByte, "$DEAD")
    assert { result.successful }
    assert { result.get == TWOBYTES(0xDEAD) }
  }

  it should "tokenize different hex sizes" in {
    val result = AssemblerLexer.parse(AssemblerLexer.tokens, """$0600
      $06
    """)
    assert { result.successful }
    assert { result.get == List(TWOBYTES(0x0600), NEWLINE(), BYTE(0x06), NEWLINE()) }
  }

  behavior of "Instruction tokenizing"

  it should "Tokenize instructions" in {
    val result = AssemblerLexer.parse(AssemblerLexer.instruction, "POP")
    assert { result.successful }
    assert { result.get == INSTRUCTION("POP") }
  }

  behavior of "Label tokenizing"

  it should "Tokenize labels" in {
    val result = AssemblerLexer.parse(AssemblerLexer.label, "Shumliduc")
    assert { result.successful }
    assert { result.get == LABEL("Shumliduc") }
  }

  behavior of "Directive tokenizing"

  it should "Tokenize directives" in {
    val result = AssemblerLexer.parse(AssemblerLexer.directive, ".BYTE")
    assert { result.successful }
    assert { result.get == DIRECTIVE(".BYTE") }
  }

  behavior of "Newline tokenizing"

  it should "Tokenize newlines" in {
    val result = AssemblerLexer.parse(AssemblerLexer.newline, "\n")
    assert { result.successful }
    assert { result.get == NEWLINE() }
  }

  behavior of "Full tokenizing"

  it should "Tokenize arbitrary programs" in {
    val result = AssemblerLexer("""*=$0600
    LDA #02 ; Load 2 into accumulator
    ADC #02 ; Add 2 to accumulator
    STA $CB ; Store accumulator in 0xCB
    """)
    assert { result.isRight }
    assert {
      result.right.get ==
        List(
          ASTERISK(),
          EQUALS(),
          TWOBYTES(0x0600),
          NEWLINE(),
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
    }
  }

  it should "Add a newline if one is missing" in {
    val result = AssemblerLexer("LDA #02 ; Load 2 into accumulator")
    assert { result.isRight }
    assert {
      result.right.get == List(
        INSTRUCTION("LDA"),
        HASH(),
        BYTE(2),
        COMMENT("; Load 2 into accumulator"),
        NEWLINE()
      )
    }
  }
}
