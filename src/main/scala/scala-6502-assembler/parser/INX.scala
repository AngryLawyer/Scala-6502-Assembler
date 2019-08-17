package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class INX() extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    List(0xE8)
  }

  def length = 1
}

object INX {
  import AssemblerParser._
  val parse = makeInstruction("INX") ^^ {
    case _ => INX()
  }
}
