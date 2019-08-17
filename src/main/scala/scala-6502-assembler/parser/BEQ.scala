package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class BEQ(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case Relative(n) => 0xF0 :: n.asRelative(index, map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object BEQ {
  import AssemblerParser._
  val parse = makeInstruction("BEQ") ~ (relative) ^^ {
    case _ ~ adm => BEQ(adm)
  }
}

