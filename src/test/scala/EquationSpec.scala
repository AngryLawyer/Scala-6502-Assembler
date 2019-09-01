package scala_6502_assembler.test

import org.scalatest._
import scala_6502_assembler.LabelResolver
import scala_6502_assembler.lexer._
import scala_6502_assembler.parser._

class EquationSpec extends FlatSpec with DiagrammedAssertions {
  behavior of "Parsing Equation"

  it should "parse a simple value" in {
    val result = AssemblerParser.equation(
      Utils.tokenReader(List(BYTE(0)))
    )

    assert { result.successful }
    assert {
      result.get == Equation.Value(0)
    }
  }

  it should "parse a simple variable" in {
    val result = AssemblerParser.equation(
      Utils.tokenReader(List(STRING("CAT")))
    )

    assert { result.successful }
    assert {
      result.get == Equation.Variable("CAT")
    }
  }

  it should "parse a simple addition" in {
    val result = AssemblerParser.equation(
      Utils.tokenReader(List(BYTE(1), OPERATOR("+"), BYTE(2)))
    )

    assert { result.successful }
    assert {
      result.get == Equation.Sum(Equation.Value(1), Equation.Value(2))
    }
  }

  it should "parse a simple subtraction" in {
    val result = AssemblerParser.equation(
      Utils.tokenReader(List(BYTE(1), OPERATOR("-"), BYTE(2)))
    )

    assert { result.successful }
    assert {
      result.get == Equation.Subtract(Equation.Value(1), Equation.Value(2))
    }
  }

  it should "parse a simple multiplication" in {
    val result = AssemblerParser.equation(
      Utils.tokenReader(List(BYTE(1), OPERATOR("*"), BYTE(2)))
    )

    assert { result.successful }
    assert {
      result.get == Equation.Multiply(Equation.Value(1), Equation.Value(2))
    }
  }

  it should "parse a simple division" in {
    val result = AssemblerParser.equation(
      Utils.tokenReader(List(BYTE(1), OPERATOR("/"), BYTE(2)))
    )

    assert { result.successful }
    assert {
      result.get == Equation.Divide(Equation.Value(1), Equation.Value(2))
    }
  }

  it should "parse a boolean AND" in {
    val result = AssemblerParser.equation(
      Utils.tokenReader(List(BYTE(1), OPERATOR("&"), BYTE(2)))
    )

    assert { result.successful }
    assert {
      result.get == Equation.And(Equation.Value(1), Equation.Value(2))
    }
  }
  
  // TODO: Handle stuff that takes more than two arguments
  //

  behavior of "Equation evaluation"

  it should "evaluate addition" in {
    val result = Equation.Sum(Equation.Value(1), Equation.Value(2)).evaluate(LabelResolver())
    assert {
      result == 3
    }
  }
}
