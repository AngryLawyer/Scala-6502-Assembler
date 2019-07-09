package scala_6502_assembler.parser
import scala_6502_assembler.LabelResolver

case class RTS() extends InstructionAST {
  def toBytes(index: Int, map: LabelResolver.LabelMap) = {
    List(0x60)
  }

  def length = 1
}

object RTS {
  import AssemblerParser._
  val parse = makeInstruction("RTS") ^^ {
    case _ => RTS()
  }
}
