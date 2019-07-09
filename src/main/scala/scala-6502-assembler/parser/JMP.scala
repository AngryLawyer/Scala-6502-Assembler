package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class JMP(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case Absolute(n) => 0x4C :: n.asShort(map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object JMP {
  import AssemblerParser._
  val parse = makeInstruction("JMP") ~ absolute ^^ {
    case _ ~ adm => JMP(adm)
  }
}
