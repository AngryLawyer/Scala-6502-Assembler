package scala_6502_assembler.parser

case class CLC() extends InstructionAST {
  def toBytes = {
    List(0x18)
  }
}

object CLC {
  import AssemblerParser._
  val parse = makeInstruction("CLC") ^^ {
    case _ => CLC()
  }
}
