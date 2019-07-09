package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class ADC(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case Immediate(n) => 0x69 :: n.asByte(map)
      case ZeroPage(n)  => 0x65 :: n.asByte(map)
      case Absolute(n)  => 0x6D :: n.asShort(map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object ADC {
  import AssemblerParser._
  val parse = (makeInstruction("ADC") ~ (zeroPage | immediate | absolute)) ^^ {
    case _ ~ adm => ADC(adm)
  }
}
