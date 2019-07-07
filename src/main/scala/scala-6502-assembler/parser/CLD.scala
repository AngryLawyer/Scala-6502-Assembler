package scala_6502_assembler.parser

case class CLD() extends InstructionAST {
  def toBytes = {
    List(0xD8)
  }
}

object CLD {
  import AssemblerParser._
  val parse = makeInstruction("CLD") ^^ {
    case _ => CLD()
  }
}
