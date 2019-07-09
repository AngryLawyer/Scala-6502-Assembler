package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class BCS(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case Relative(n) => 0xB0 :: n.asRelative(index, map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object BCS {
  import AssemblerParser._
  val parse = makeInstruction("BCS") ~ relative ^^ {
    case _ ~ adm => BCS(adm)
  }
}
