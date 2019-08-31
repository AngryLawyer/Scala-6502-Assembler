package scala_6502_assembler.parser

trait Equation
object Equation {
  case class Value(data: Int) extends Equation
  case class Variable(data: String) extends Equation
  case class Dyadic(operator: Operator, left: Equation, right: Equation) extends Equation
}
