package scala_6502_assembler.parser

case class RTS() extends InstructionAST {
  def toBytes = {
    List(0x60)
  }
}

object RTS {
  import AssemblerParser._
  val parse = makeInstruction("RTS") ^^ {
    case _ => RTS()
  }
}
