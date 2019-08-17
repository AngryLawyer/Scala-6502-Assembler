package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class CMP(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case Immediate(n) => 0xC9 :: n.asByte(map)
      case ZeroPage(n)  => 0xC5 :: n.asByte(map)
      case ZeroPageX(n)  => 0xD5 :: n.asByte(map)
      case Absolute(n)  => 0xCD :: n.asShort(map)
      case AbsoluteX(n)  => 0xDD :: n.asShort(map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object CMP {
  import AssemblerParser._
  val parse = makeInstruction("CMP") ~ (zeroPageX | zeroPage | immediate | absoluteX | absolute) ^^ {
    case _ ~ adm => CMP(adm)
  }
}
