package scala_6502_assembler.parser

import scala_6502_assembler.LabelResolver
import scala.util.parsing.input.Positional

case class UnsupportedAddressingModeException(private val message: String = "", private val cause: Throwable = None.orNull) extends Exception(message, cause)

case class NUMBER(value: Int) extends Positional
case class ORIGIN(value: Int) extends Positional


case class Label(name: String) extends Positional
case class Char(name: String) extends Positional
case class Instruction(name: String) extends Positional
case class BytesDirective(data: List[BytesContent]) extends Positional

sealed trait BytesContent
case class BytesContentByte(data: Int) extends BytesContent
case class BytesContentQuote(data: String) extends BytesContent
case class BytesContentVariable(data: String) extends BytesContent

case class AddressingModeValue(equation: Equation) {
  def asByte(map: LabelResolver.LabelMap): List[Int] = {
    List(equation.evaluate(map) & 0xFF)
  }
  def asShort(map: LabelResolver.LabelMap): List[Int] = {
    val data = equation.evaluate(map)
    List(data & 0xFF, (data >> 8) & 0xFF)
  }
  def asRelative(index: Int, map: LabelResolver.LabelMap): List[Int] = {
    val labelLoc = equation.evaluate(map)
    var jump = labelLoc - index
    List((jump + 127) & 0xFF)
  }
}

sealed trait AddressingMode extends Positional {
  def length: Int
}
case class Immediate(value: AddressingModeValue) extends AddressingMode {
  def length = 1
}
case class ZeroPage(value: AddressingModeValue) extends AddressingMode {
  def length = 1
}
case class ZeroPageX(value: AddressingModeValue) extends AddressingMode {
  def length = 1
}
case class Absolute(value: AddressingModeValue) extends AddressingMode {
  def length = 2
}
case class AbsoluteX(value: AddressingModeValue) extends AddressingMode {
  def length = 2
}
case class Relative(value: AddressingModeValue) extends AddressingMode {
  def length = 1
}

trait InstructionAST extends Positional {
  def toBytes(index: Int, map: LabelResolver.LabelMap): List[Int]
  def length: Int
}

sealed trait Line extends Positional {
  def toBytes(index: Int, map: LabelResolver.LabelMap): List[Int]
}

case class InstructionLine(label: Option[Label], instruction: InstructionAST, next: Option[Line])
    extends Line {
  def toBytes(index: Int, map: LabelResolver.LabelMap): List[Int] = {
    val myInstruction = instruction.toBytes(index, map)
    next match {
      case Some(nextLine) => myInstruction ++ nextLine.toBytes(index + instruction.length, map)
      case _              => myInstruction
    }
  }
}

case class CommentedLine(label: Option[Label], next: Option[Line]) extends Line {
  def toBytes(index: Int, map: LabelResolver.LabelMap): List[Int] = {
    next match {
      case Some(nextLine) => nextLine.toBytes(index, map)
      case _              => List()
    }
  }
}

case class VariableLine(name: String, value: Int, next: Option[Line]) extends Line {
  def toBytes(index: Int, map: LabelResolver.LabelMap): List[Int] = {
    next match {
      case Some(nextLine) => nextLine.toBytes(index, map)
      case _              => List()
    }
  }
}

case class BytesLine(label: Option[Label], data: List[BytesContent], next: Option[Line])
    extends Line {
  private def contentsToBytes(map: LabelResolver.LabelMap): List[Int] = data.collect {
    case BytesContentByte(b) => List(b)
    case BytesContentQuote(q) => q.map(_.toInt)
    case BytesContentVariable(v) => List(map(v) & 0xFF)
  }.flatten

  def toBytes(index: Int, map: LabelResolver.LabelMap): List[Int] = {
    val dataParsed = contentsToBytes(map)
    next match {
      case Some(nextLine) => dataParsed ++ nextLine.toBytes(index + dataParsed.length, map)
      case _              => dataParsed
    }
  }
}

sealed trait SectionOrEnd extends Positional
case class Section(startAddress: Int, line: Line, next: Option[Section])
    extends SectionOrEnd {
  def toVirtual6502(map: LabelResolver.LabelMap): String = {
    val bytes = line.toBytes(startAddress, map).map(byte => f"$byte%02x").mkString(" ")
    val result = f":$startAddress%04x  $bytes"
    next match {
      case Some(nextSection) => (if (bytes.length == 0) { "" } else { result ++ "\n" }) ++ nextSection.toVirtual6502(map)
      case _                 => result
    }
  }

  private def toXexInner(firstSectionStart: Int, map: LabelResolver.LabelMap): List[Int] = {
    val header = List(0xFF, 0xFF)
    val bytes = line.toBytes(startAddress, map)
    val startAddrBytes = List(startAddress & 0xFF, (startAddress >> 8) & 0xFF)
    val endAddr = bytes.length - startAddress - 1
    val endAddrBytes = List(endAddr & 0xFF, (endAddr >> 8) & 0xFF)
    val result = header ++ startAddrBytes ++ endAddrBytes ++ bytes

    next match {
      case Some(nextSection) => (if (bytes.length == 0) { List() } else { result }) ++ nextSection.toXexInner(firstSectionStart, map)
      case _ =>
        result ++ List(0xFF, 0xFF, 0xE2, 0x02, 0xE3, 0x02) ++ List(firstSectionStart & 0xFF, (firstSectionStart >> 8) & 0xFF)
    }
  }

  def toXex(map: LabelResolver.LabelMap): List[Int] = {
    toXexInner(startAddress, map)
  }

  def toBytes(map: LabelResolver.LabelMap): List[Int] = {
    val myLine = line.toBytes(startAddress, map)
    next match {
      case Some(nextSection) => myLine ++ nextSection.toBytes(map)
      case _                 => myLine
    }
  }
}
case class End() extends SectionOrEnd
