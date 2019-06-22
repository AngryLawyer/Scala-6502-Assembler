package scala_6502_assembler.lexer

import scala.util.parsing.combinator._
import scala_6502_assembler.error._

object AssemblerLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def comment: Parser[COMMENT] = positioned {
    ";[ -~]+".r ^^ { str =>
      COMMENT(str)
    }
  }

  def hash: Parser[HASH] = positioned {
    "#" ^^ { _ =>
      HASH()
    }
  }

  def decimal: Parser[NUMBER] = positioned { "[0-9]+".r ^^ { str =>
    NUMBER(str.toInt)
  }}

  def hexadecimal: Parser[NUMBER] = positioned { """\$[0-9A-F]+""".r ^^ { str =>
    NUMBER(Integer.parseInt(str.substring(1), 16))
  }}

  def instruction: Parser[INSTRUCTION] = positioned { "[A-Z]{3}".r ^^ { str =>
    INSTRUCTION(str)
  }}

  def label: Parser[LABEL] = positioned { "[A-Za-z]+".r ^^ { str =>
    LABEL(str)
  }}

  def directive: Parser[DIRECTIVE] = positioned { """\.[A-Z]+""".r ^^ { str =>
    DIRECTIVE(str)
  }}

  def newline: Parser[NEWLINE] = positioned { "\n" ^^ { _ =>
    NEWLINE()
  }}

  def tokens: Parser[List[AssemblerToken]] = {
    phrase(
      rep1(
        comment | hash | decimal | hexadecimal | instruction | label | directive | newline
      )
    ) ^^ { addNewlineIfNeeded(_) }
  }

  def apply(code: String): Either[AssemblerLexerError, List[AssemblerToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next)  => Left(AssemblerLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  private def addNewlineIfNeeded(
      tokens: List[AssemblerToken]
  ): List[AssemblerToken] = {
    tokens.lastOption match {
      case Some(NEWLINE()) => tokens
      case Some(_)       => tokens :+ NEWLINE()
      case _             => tokens
    }
  }
}

