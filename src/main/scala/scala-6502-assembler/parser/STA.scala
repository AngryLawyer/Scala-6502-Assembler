package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class STA(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case ZeroPage(n) => 0x85 :: n.asByte(map)
      case ZeroPageX(n) => 0x95 :: n.asByte(map)
      case Absolute(n)  => 0x8D :: n.asShort(map)
      case AbsoluteX(n)  => 0x9D :: n.asShort(map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object STA {
  import AssemblerParser._
  val parse = (makeInstruction("STA") ~ (zeroPageX | zeroPage | absoluteX | absolute)) ^^ {
    case _ ~ adm => STA(adm)
  }
}
