import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.{AssemblerLexer, COMMENT, NUMBER, INSTRUCTION, LABEL, DIRECTIVE}


class LexerSpec extends FlatSpec with DiagrammedAssertions {
  behavior of "Comment Tokenizing"

  it should "tokenize comments" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.comment, "; Here is a comment")
    assert { result.successful }
    val internal = result.get match {
      case COMMENT(string) => Some(string)
      case _ => None
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
      case _ => None
    }
    assert { internal.get == 12345 }
  }

  it should "tokenize decimals" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.decimal, "#12345")
    assert { result.successful }
    val internal = result.get match {
      case NUMBER(n) => Some(n)
      case _ => None
    }
    assert { internal.get == 12345 }
  }

  it should "tokenize hex" in {
    val lexer = new AssemblerLexer
    val result = lexer.parse(lexer.hexadecimal, "$DEAD")
    assert { result.successful }
    val internal = result.get match {
      case NUMBER(n) => Some(n)
      case _ => None
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
      case _ => None
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
      case _ => None
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
      case _ => None
    }
    assert { internal.get == ".BYTE" }
  }
}
