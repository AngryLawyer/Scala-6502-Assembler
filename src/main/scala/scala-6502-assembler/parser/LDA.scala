package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class LDA(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case Immediate(n) => 0xA9 :: n.asByte(map)
      case ZeroPage(n)  => 0xA5 :: n.asByte(map)
      case Absolute(n)  => 0xAD :: n.asShort(map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object LDA {
  import AssemblerParser._
  val parse = makeInstruction("LDA") ~ (zeroPage | immediate | absolute) ^^ {
    case _ ~ adm => LDA(adm)
  }
}
