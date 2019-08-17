package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class LDX(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case Immediate(n) => 0xA2 :: n.asByte(map)
      case ZeroPage(n) => 0xA6 :: n.asByte(map)
      case Absolute(n)  => 0xAE :: n.asShort(map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object LDX {
  import AssemblerParser._
  val parse = (makeInstruction("LDX") ~ (immediate | zeroPage | absolute)) ^^ {
    case _ ~ adm => LDX(adm)
  }
}
