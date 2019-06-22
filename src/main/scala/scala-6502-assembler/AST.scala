package scala_6502_assembler.parser

import scala.util.parsing.input.Positional

sealed trait AddressingMode extends Positional
case class Immediate(value: Integer) extends AddressingMode
case class Relative(value: Integer) extends AddressingMode

sealed trait InstructionAST extends Positional
case class LDA(value: AddressingMode) extends InstructionAST
case class ADC(value: AddressingMode) extends InstructionAST
case class STA(value: AddressingMode) extends InstructionAST

sealed trait AssemblerAST  extends Positional
case class Line(instruction: InstructionAST) extends AssemblerAST
