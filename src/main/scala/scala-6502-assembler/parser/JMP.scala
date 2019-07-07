package scala_6502_assembler.parser

case class JMP(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Absolute(n) => List(0x4C, n & 0xFF, (n >> 8) & 0xFF)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}

object JMP {
  import AssemblerParser._
  val parse = makeInstruction("JMP") ~ absolute ^^ {
    case _ ~ adm => JMP(adm)
  }
}
