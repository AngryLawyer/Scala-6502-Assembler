package scala_6502_assembler.parser

case class ADC(value: AddressingMode) extends InstructionAST {
  def toBytes = {
    value match {
      case Immediate(n) => List(0x69, n)
      case ZeroPage(n)  => List(0x65, n)
      case Absolute(n)  => List(0x6D, n & 0xff, (n >> 8) & 0xff)
      case _ => throw new UnsupportedAddressingModeException
    }
  }
}

object ADC {
  import AssemblerParser._
  val parse = (makeInstruction("ADC") ~ (zeroPage | immediate | absolute)) ^^ {
    case _ ~ adm => ADC(adm)
  }
}
