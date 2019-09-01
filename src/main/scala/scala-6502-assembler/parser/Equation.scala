package scala_6502_assembler.parser

import scala.util.parsing.input.Positional

import scala_6502_assembler.LabelResolver
import scala_6502_assembler.lexer.OPERATOR

trait Equation extends Positional {
  def evaluate(vars: LabelResolver.LabelMap): Int
}

object Equation {
  case class Value(data: Int) extends Equation {
    def evaluate(vars: LabelResolver.LabelMap): Int = data
  }

  case class Variable(data: String) extends Equation {
    def evaluate(vars: LabelResolver.LabelMap): Int = vars(data)
  }

  case class Sum(left: Equation, right: Equation) extends Equation {
    def evaluate(vars: LabelResolver.LabelMap): Int = left.evaluate(vars) + right.evaluate(vars)
  }

  case class Subtract(left: Equation, right: Equation) extends Equation {
    def evaluate(vars: LabelResolver.LabelMap): Int = left.evaluate(vars) - right.evaluate(vars)
  }

  case class Multiply(left: Equation, right: Equation) extends Equation {
    def evaluate(vars: LabelResolver.LabelMap): Int = left.evaluate(vars) * right.evaluate(vars)
  }

  case class Divide(left: Equation, right: Equation) extends Equation {
    def evaluate(vars: LabelResolver.LabelMap): Int = left.evaluate(vars) / right.evaluate(vars)
  }

  case class And(left: Equation, right: Equation) extends Equation {
    def evaluate(vars: LabelResolver.LabelMap): Int = left.evaluate(vars) & right.evaluate(vars)
  }

}
