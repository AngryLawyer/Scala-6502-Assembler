package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class JSR(value: AddressingMode) extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    value match {
      case Absolute(n) => 0x20 :: n.asShort(map)
      case _ => throw new UnsupportedAddressingModeException
    }
  }

  def length = value.length + 1
}

object JSR {
  import AssemblerParser._
  val parse = makeInstruction("JSR") ~ absolute ^^ {
    case _ ~ adm => JSR(adm)
  }
}
