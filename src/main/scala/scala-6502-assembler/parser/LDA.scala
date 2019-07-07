package scala_6502_assembler.parser

case class LDA(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Immediate(n) => List(0xA9, n)
      case ZeroPage(n)  => List(0xA5, n)
      case Absolute(n)  => List(0xAD, n & 0xFF, (n >> 8) & 0xFF)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}

object LDA {
  import AssemblerParser._
  val parse = makeInstruction("LDA") ~ (zeroPage | immediate | absolute) ^^ {
    case _ ~ adm => LDA(adm)
  }
}
