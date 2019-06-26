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

case class Line(instruction: InstructionAST, next: Option[Line]) extends Positional {
  def toBytes: List[Integer] = {
    val myInstruction = instruction.toBytes
    next match {
      case Some(nextLine) => myInstruction ++ nextLine.toBytes
      case _ => myInstruction
    }
  }
}


case class Section(startAddress: Integer, line: Line, next: Option[Section]) extends Positional {
  def toBytes: List[Integer] = {
    val myLine = line.toBytes
    next match {
      case Some(nextSection) => myLine ++ nextSection.toBytes
      case _ => myLine
    }
  }
}
