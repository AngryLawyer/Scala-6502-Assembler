package scala_6502_assembler
import scala_6502_assembler.parser._

object LabelResolver {
  type LabelMap = Map[String, Int]

  def getLabelMap(code: Section): Map[String, Int] = {
    getLabelMapSection(code, Map())
  }

  private def getLabelMapSection(code: Section, map: LabelMap): LabelMap = {
    val updatedMap = getLabelMapLine(code.line, code.startAddress, map)
    code.next match {
      case Some(next) => getLabelMapSection(next, updatedMap)
      case None => updatedMap
    }
  }

  private def getLabelMapLine(code: Line, index: Int, map: LabelMap): LabelMap = {
    code match {
      case VariableLine(label, value, next) => {
        val updatedMap = map + (label -> value)
        next match {
          case Some(n) => getLabelMapLine(n, index, updatedMap)
          case None => updatedMap
        }
      }
      case _ => {
        val (label, instructionLength, next) = code match {
          case CommentedLine(label, next) => (label, 0, next)
          case InstructionLine(label, instruction, next) => (label, instruction.length, next)
        }
        val updatedMap = label match {
          case Some(Label(l)) => map + (l -> index)
          case None => map
        }
        next match {
          case Some(n) => getLabelMapLine(n, index + instructionLength, updatedMap)
          case None => updatedMap
        }
      }
    }
  }
}
