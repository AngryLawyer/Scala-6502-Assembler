package scala_6502_assembler.parser

import scala.util.parsing.input.Positional

case class NUMBER(value: Int) extends Positional

sealed trait AddressingMode extends Positional
case class Immediate(value: Int) extends AddressingMode
case class ZeroPage(value: Int) extends AddressingMode

sealed trait InstructionAST extends Positional {
  def toBytes: List[Int];
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
  def toBytes: List[Int] = {
    val myInstruction = instruction.toBytes
    next match {
      case Some(nextLine) => myInstruction ++ nextLine.toBytes
      case _ => myInstruction
    }
  }
}


case class Section(startAddress: Int, line: Line, next: Option[Section]) extends Positional {
  def toVirtual6502: String = {
    val bytes = line.toBytes.map(byte => f"$byte%02x").mkString(" ")
    val result = f":$startAddress%04x  $bytes"
    next match {
      case Some(nextSection) => result ++ "\n" ++ nextSection.toVirtual6502
      case _ => result
    }
  }

  def toXex: List[Int] = {
    val header = List(0xFF, 0xFF)
    val bytes = line.toBytes
    val startAddrBytes = List(startAddress & 0xFF, (startAddress >> 8) & 0xFF)
    val endAddr = bytes.length - startAddress - 1
    val endAddrBytes = List(endAddr & 0xFF, (endAddr >> 8) & 0xFF)
    header ++ startAddrBytes ++ endAddrBytes ++ bytes ++ List(0xFF, 0xFF, 0xE2, 0x02, 0xE3, 0x02) ++ startAddrBytes
  }

  def toBytes: List[Int] = {
    val myLine = line.toBytes
    next match {
      case Some(nextSection) => myLine ++ nextSection.toBytes
      case _ => myLine
    }
  }
}
