package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class CLD() extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    List(0xD8)
  }

  def length = 1
}

object CLD {
  import AssemblerParser._
  val parse = makeInstruction("CLD") ^^ {
    case _ => CLD()
  }
}
