package scala_6502_assembler

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}

sealed trait AssemblerToken

case class COMMENT(text: String) extends AssemblerToken
case class NUMBER(value: Int) extends AssemblerToken
case class INSTRUCTION(text: String) extends AssemblerToken
case class LABEL(text: String) extends AssemblerToken
case class DIRECTIVE(text: String) extends AssemblerToken
case object NEWLINE extends AssemblerToken
case object HASH extends AssemblerToken

trait AssemblerCompilationError
case class AssemblerLexerError(msg: String) extends AssemblerCompilationError

object AssemblerLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def comment: Parser[COMMENT] = ";[ -~]+".r ^^ { str =>
    COMMENT(str)
  }

  def hash = "#" ^^ { _ =>
    HASH
  }

  def decimal: Parser[NUMBER] = "[0-9]+".r ^^ { str =>
    NUMBER(str.toInt)
  }

  def hexadecimal: Parser[NUMBER] = """\$[0-9A-F]+""".r ^^ { str =>
    NUMBER(Integer.parseInt(str.substring(1), 16))
  }

  def instruction: Parser[INSTRUCTION] = "[A-Z]{3}".r ^^ { str =>
    INSTRUCTION(str)
  }
  def label: Parser[LABEL] = "[A-Za-z]+".r ^^ { str =>
    LABEL(str)
  }
  def directive: Parser[DIRECTIVE] = """\.[A-Z]+""".r ^^ { str =>
    DIRECTIVE(str)
  }
  def newline = "\n" ^^ { _ =>
    NEWLINE
  }

  def tokens: Parser[List[AssemblerToken]] = {
    phrase(
      rep1(
        comment | hash | decimal | hexadecimal | instruction | label | directive | newline
      )
    ) ^^ { addNewlineIfNeeded(_) }
  }

  def apply(code: String): Either[AssemblerLexerError, List[AssemblerToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next)  => Left(AssemblerLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }

  private def addNewlineIfNeeded(
      tokens: List[AssemblerToken]
  ): List[AssemblerToken] = {
    tokens.lastOption match {
      case Some(NEWLINE) => tokens
      case Some(_)       => tokens :+ NEWLINE
      case _             => tokens
    }
  }
}

sealed trait AddressingMode
case class Immediate(value: Integer) extends AddressingMode
case class Relative(value: Integer) extends AddressingMode

sealed trait InstructionAST
case class LDA(value: AddressingMode) extends InstructionAST

sealed trait AssemblerAST
case class Line(instruction: InstructionAST) extends AssemblerAST

object AssemblerParser extends Parsers {
  override type Elem = AssemblerToken

  class AssemblerTokenReader(tokens: Seq[AssemblerToken])
      extends Reader[AssemblerToken] {
    override def first: AssemblerToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[AssemblerToken] =
      new AssemblerTokenReader(tokens.tail)
  }

  def instructionToken: Parser[INSTRUCTION] = {
    accept("instruction", { case ins @ INSTRUCTION(_) => ins })
  }

  def number: Parser[NUMBER] = {
    accept("number", { case n @ NUMBER(_) => n })
  }

  def comment: Parser[COMMENT] = {
    accept("comment", { case c @ COMMENT(_) => c })
  }

  def instruction: Parser[InstructionAST] = {
    (instructionToken ~ number) ^^ {
      case INSTRUCTION("LDA") ~ NUMBER(n) => LDA(Immediate(n))
    }
  }

  def line: Parser[AssemblerAST] = {
    (instruction ~ comment ~ NEWLINE) ^^ {
      case inst ~ _ ~ _ => Line(inst)
    }
  }

  def program: Parser[AssemblerAST] = {
    phrase(line)
  }
}
