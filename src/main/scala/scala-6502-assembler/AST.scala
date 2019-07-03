package scala_6502_assembler.parser

import scala.util.parsing.input.Positional

case class UnsupportedAddressingModeException(private val message: String = "", private val cause: Throwable = None.orNull) extends Exception(message, cause)

case class NUMBER(value: Int) extends Positional
case class ORIGIN(value: Int) extends Positional

case class Label(name: String) extends Positional
case class Instruction(name: String) extends Positional

sealed trait AddressingMode extends Positional
case class Immediate(value: Int) extends AddressingMode
case class ZeroPage(value: Int) extends AddressingMode
case class Absolute(value: Int) extends AddressingMode

sealed trait InstructionAST extends Positional {
  def toBytes: List[Int];
}

case class JMP(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Absolute(n) => List(0x4C, n & 0xFF, (n >> 8) & 0xFF)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}

case class CLC() extends InstructionAST {
  def toBytes = {
    List(0x18)
  }
}

case class CLD() extends InstructionAST {
  def toBytes = {
    List(0xD8)
  }
}

case class RTS() extends InstructionAST {
  def toBytes = {
    List(0x60)
  }
}

case class LDA(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Immediate(n) => List(0xA9, n)
      case ZeroPage(n)  => List(0xA5, n)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}
case class ADC(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Immediate(n) => List(0x69, n)
      case ZeroPage(n)  => List(0x65, n)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}
case class STA(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case ZeroPage(n) => List(0x85, n)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}

sealed trait Line extends Positional {
  def toBytes: List[Int]
}

case class InstructionLine(label: Option[Label], instruction: InstructionAST, next: Option[Line])
    extends Line {
  def toBytes: List[Int] = {
    val myInstruction = instruction.toBytes
    next match {
      case Some(nextLine) => myInstruction ++ nextLine.toBytes
      case _              => myInstruction
    }
  }
}

case class CommentedLine(label: Option[Label], next: Option[Line]) extends Line {
  def toBytes: List[Int] = {
    next match {
      case Some(nextLine) => nextLine.toBytes
      case _              => List()
    }
  }
}

sealed trait SectionOrEnd extends Positional
case class Section(startAddress: Int, line: Line, next: Option[Section])
    extends SectionOrEnd {
  def toVirtual6502: String = {
    val bytes = line.toBytes.map(byte => f"$byte%02x").mkString(" ")
    val result = f":$startAddress%04x  $bytes"
    next match {
      case Some(nextSection) => result ++ "\n" ++ nextSection.toVirtual6502
      case _                 => result
    }
  }

  private def toXexInner(start: Int): List[Int] = {
    val header = List(0xFF, 0xFF)
    val bytes = line.toBytes
    val startAddrBytes = List(startAddress & 0xFF, (startAddress >> 8) & 0xFF)
    val endAddr = bytes.length - startAddress - 1
    val endAddrBytes = List(endAddr & 0xFF, (endAddr >> 8) & 0xFF)
    val result = header ++ startAddrBytes ++ endAddrBytes ++ bytes

    next match {
      case Some(nextSection) => result ++ nextSection.toXexInner(start)
      case _ =>
        result ++ List(0xFF, 0xFF, 0xE2, 0x02, 0xE3, 0x02) ++ startAddrBytes
    }
  }

  def toXex: List[Int] = {
    toXexInner(startAddress)
  }

  def toBytes: List[Int] = {
    val myLine = line.toBytes
    next match {
      case Some(nextSection) => myLine ++ nextSection.toBytes
      case _                 => myLine
    }
  }
}
case class End() extends SectionOrEnd
