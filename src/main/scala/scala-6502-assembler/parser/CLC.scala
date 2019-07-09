package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class CLC() extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    List(0x18)
  }

  def length = 1
}

object CLC {
  import AssemblerParser._
  val parse = makeInstruction("CLC") ^^ {
    case _ => CLC()
  }
}
