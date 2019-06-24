package scala_6502_assembler.parser

import scala.util.parsing.input.Positional

sealed trait AddressingMode extends Positional
case class Immediate(value: Integer) extends AddressingMode
case class ZeroPage(value: Integer) extends AddressingMode

sealed trait InstructionAST extends Positional {
  def toBytes: List[Integer];
}

case class LDA(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Immediate(n) => List(0xA9, n)
      case ZeroPage(n) => List(0xA5, n)
    }
  }
}
case class ADC(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Immediate(n) => List(0x69, n)
      case ZeroPage(n) => List(0x65, n)
    }
  }
}
case class STA(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case ZeroPage(n) => List(0x85, n)
      case _ => List()
    }
  }
}

sealed trait AssemblerAST extends Positional {
  def toBytes: List[Integer];
}
case class Line(instruction: InstructionAST) extends AssemblerAST {
  def toBytes = {
    instruction.toBytes
  }
}
