package scala_6502_assembler.test

import org.scalatest._
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
}
