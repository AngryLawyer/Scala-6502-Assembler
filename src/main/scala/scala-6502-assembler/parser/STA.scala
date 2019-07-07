package scala_6502_assembler.parser

case class STA(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case ZeroPage(n) => List(0x85, n)
      case Absolute(n)  => List(0x8D, n & 0xff, (n >> 8) & 0xff)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}

object STA {
  import AssemblerParser._
  val parse = (makeInstruction("STA") ~ zeroPage) ^^ {
    case _ ~ adm => STA(adm)
  }
}
