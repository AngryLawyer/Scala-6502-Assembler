package scala_6502_assembler.parser

sealed trait AddressingMode
case class Immediate(value: Integer) extends AddressingMode
case class Relative(value: Integer) extends AddressingMode

sealed trait InstructionAST
case class LDA(value: AddressingMode) extends InstructionAST
case class ADC(value: AddressingMode) extends InstructionAST
case class STA(value: AddressingMode) extends InstructionAST

sealed trait AssemblerAST
case class Line(instruction: InstructionAST) extends AssemblerAST
