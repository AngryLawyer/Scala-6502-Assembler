package scala_6502_assembler.parser

import scala.util.parsing.input.Positional

import scala_6502_assembler.lexer.OPERATOR

trait Equation extends Positional
object Equation {
  case class Value(data: Int) extends Equation
  case class Variable(data: String) extends Equation
  case class Dyadic(operator: OPERATOR, left: Equation, right: Equation) extends Equation
}
