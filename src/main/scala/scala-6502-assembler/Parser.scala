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

  def instructionToken: Parser[INSTRUCTION] = {
    accept("instruction", { case ins @ INSTRUCTION(_) => ins })
  }

  def number: Parser[NUMBER] = {
    accept("number", { case n @ NUMBER(_) => n })
  }

  def comment: Parser[COMMENT] = {
    accept("comment", { case c @ COMMENT(_) => c })
  }

  def addressMode: Parser[AddressingMode] = {
    val relative = (number) ^^ {
      case NUMBER(n) => Relative(n)
    }
    val immediate = (HASH ~ number) ^^ {
      case _ ~ NUMBER(n) => Immediate(n)
    }
    relative | immediate
  }

  def instruction: Parser[InstructionAST] = {
    (instructionToken ~ addressMode) ^^ {
      case INSTRUCTION("LDA") ~ adm => LDA(adm)
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
