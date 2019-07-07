package scala_6502_assembler.parser

case class BCS(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Relative(n) => List(0xB0, n)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}

object BCS {
  import AssemblerParser._
  val parse = makeInstruction("BCS") ~ relative ^^ {
    case _ ~ adm => BCS(adm)
  }
}
