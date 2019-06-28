package scala_6502_assembler.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala_6502_assembler.lexer._
import scala_6502_assembler.parser._
import scala_6502_assembler.error._

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

  def apply(
      tokens: Seq[AssemblerToken]
  ): Either[AssemblerParserError, Section] = {
    val reader = new AssemblerTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) =>
        Left(
          AssemblerParserError(Location(next.pos.line, next.pos.column), msg)
        )
      case Success(result, next) => Right(result)
    }
  }

  def makeInstruction(text: String): Parser[INSTRUCTION] = positioned {
    accept(text, {
      case ins @ INSTRUCTION(parsedText) if parsedText == text => ins
    })
  }

  def comment: Parser[COMMENT] = positioned {
    accept("comment", { case c @ COMMENT(_) => c })
  }

  def byte: Parser[BYTE] = positioned {
    accept("byte", { case n @ BYTE(_) => n })
  }

  def twoBytes: Parser[TWOBYTES] = positioned {
    accept("twoBytes", { case n @ TWOBYTES(_) => n })
  }

  def number: Parser[NUMBER] = positioned {
    val myByte = byte ^^ {
      case BYTE(n) => NUMBER(n)
    }
    val myTwoBytes = twoBytes ^^ {
      case TWOBYTES(n) => NUMBER(n)
    }
    myByte | myTwoBytes
  }

  def zeroPage: Parser[AddressingMode] = positioned {
    byte ^^ {
      case BYTE(n) => ZeroPage(n)
    }
  }

  def immediate: Parser[AddressingMode] = positioned {
    HASH() ~ number ^^ {
      case _ ~ NUMBER(n) => Immediate(n)
    }
  }

  def instruction: Parser[InstructionAST] = positioned {
    val lda = makeInstruction("LDA") ~ (zeroPage | immediate) ^^ {
      case _ ~ adm => LDA(adm)
    }
    val adc = (makeInstruction("ADC") ~ (zeroPage | immediate)) ^^ {
      case _ ~ adm => ADC(adm)
    }
    val sta = (makeInstruction("STA") ~ zeroPage) ^^ {
      case _ ~ adm => STA(adm)
    }

    lda | adc | sta
  }

  def line: Parser[Line] = positioned {
    (instruction ~ opt(comment) ~ NEWLINE() ~ opt(line)) ^^ {
      case inst ~ _ ~ _ ~ next => Line(inst, next)
    }
  }

  def section: Parser[Section] = positioned {
    (line ~ opt(section)) ^^ {
      case lineData ~ next => Section(0, lineData, next)
    }
  }

  def program: Parser[Section] = positioned {
    phrase(section)
  }
}
