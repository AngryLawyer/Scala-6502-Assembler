package scala_6502_assembler.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala_6502_assembler.lexer._
import scala_6502_assembler.parser._
import scala_6502_assembler.error._

object AssemblerParser extends Parsers {
  override type Elem = AssemblerToken

  class AssemblerTokenReader(tokens: Seq[AssemblerToken])
      extends Reader[AssemblerToken] {
    override def first: AssemblerToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[AssemblerToken] =
      new AssemblerTokenReader(tokens.tail)
  }

  def apply(
      tokens: Seq[AssemblerToken]
  ): Either[AssemblerParserError, Section] = {
    val reader = new AssemblerTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) =>
        Left(
          AssemblerParserError(Location(next.pos.line, next.pos.column), msg)
        )
      case Success(result, next) => Right(result)
    }
  }

  def makeInstruction(text: String): Parser[Instruction] = positioned {
    accept(text, {
      case STRING(parsedText) if parsedText == text => Instruction(parsedText)
    })
  }

  def comment: Parser[COMMENT] = positioned {
    accept("comment", { case c @ COMMENT(_) => c })
  }

  def makeDirective(text: String): Parser[DIRECTIVE] = positioned {
    accept(text, {
      case ins @ DIRECTIVE(parsedText) if parsedText == text => ins
    })
  }

  def makeChar(text: String): Parser[Char] = positioned {
    accept(text, {
      case STRING(parsedText) if parsedText == text => Char(parsedText)
    })
  }

  def byte: Parser[BYTE] = positioned {
    accept("byte", { case n @ BYTE(_) => n })
  }

  def comma: Parser[COMMA] = positioned {
    accept("comma", { case c @ COMMA() => c })
  }

  def twoBytes: Parser[TWOBYTES] = positioned {
    accept("twoBytes", { case n @ TWOBYTES(_) => n })
  }

  def number: Parser[NUMBER] = positioned {
    val myByte = byte ^^ {
      case BYTE(n) => NUMBER(n)
    }
    val myTwoBytes = twoBytes ^^ {
      case TWOBYTES(n) => NUMBER(n)
    }
    myByte | myTwoBytes
  }

  def quote: Parser[QUOTE] = positioned {
    accept("quote", { case q @ QUOTE(_) => q })
  }

  def equation: Parser[Equation] = positioned {
    number ^^ {
      case NUMBER(b) => Equation.Value(b)
    }
  }

  def zeroPage: Parser[AddressingMode] = positioned {
    // FIXME: Handle labels
    byte ^^ {
      case BYTE(n) => ZeroPage(AddressingModeNumber(n))
    }
  }

  def zeroPageX: Parser[AddressingMode] = positioned {
    // FIXME: Handle labels
    byte ~ comma ~ makeChar("X") ^^ {
      case BYTE(n) ~ _ ~ _ => ZeroPageX(AddressingModeNumber(n))
    }
  }

  def relative: Parser[AddressingMode] = positioned {
    (byte | label) ^^ {
      case BYTE(n) => Relative(AddressingModeNumber(n))
      case Label(n) => Relative(AddressingModeLabel(n))
    }
  }

  def absolute: Parser[AddressingMode] = positioned {
    (twoBytes | label) ^^ {
      case TWOBYTES(n) => Absolute(AddressingModeNumber(n))
      case Label(n) => Absolute(AddressingModeLabel(n))
    }
  }

  def absoluteX: Parser[AddressingMode] = positioned {
    (twoBytes | label) ~ comma ~ makeChar("X") ^^ {
      case TWOBYTES(n) ~ _ ~ _ => Absolute(AddressingModeNumber(n))
      case Label(n) ~ _ ~ _ => AbsoluteX(AddressingModeLabel(n))
    }
  }

  def immediate: Parser[AddressingMode] = positioned {
    (HASH() ~ (byte | label)) ^^ {
      case _ ~ BYTE(n) => Immediate(AddressingModeNumber(n))
      case _ ~ Label(n) => Immediate(AddressingModeLabel(n))
    }
  }

  def bytesDirective: Parser[BytesDirective] = positioned {
    makeDirective(".BYTE") ~ ((byte | quote | label) ~ comma).* ~ (byte | quote | label).?  ^^ {
      case _ ~ items ~ trailing => {
        val content = items.collect {
            case (BYTE(x) ~ _) => BytesContentByte(x)    
            case (QUOTE(x) ~ _) => BytesContentQuote(x)
            case (Label(x) ~ _) => BytesContentVariable(x)
        }
        val contentPlusLast = trailing match {
          case Some(BYTE(x)) => content :+ BytesContentByte(x)
          case Some(QUOTE(x)) => content :+ BytesContentQuote(x)
          case Some(Label(x)) => content :+ BytesContentVariable(x)
          case None => content
        }

        BytesDirective(contentPlusLast)
      }
    }
  }

  def label: Parser[Label] = positioned {
    accept("label", { case STRING(l) => Label(l) })
  }

  def end: Parser[End] = positioned {
    makeDirective(".END") ~ opt(comment) ~ NEWLINE() ~ opt(section) ^^ {
      case _ ~ _ ~ _ ~ _ => End()
    }
  }

  def instruction: Parser[InstructionAST] = positioned {
    BCS.parse | CLC.parse | CLD.parse | RTS.parse | JMP.parse | LDA.parse | ADC.parse | STA.parse | LDX.parse | CMP.parse | BEQ.parse | INX.parse
  }

  def instructionLine: Parser[Line] = positioned {
    def lineWithoutLabel = (instruction ~ opt(comment) ~ NEWLINE() ~ opt(line)) ^^ {
      case inst ~ _ ~ _ ~ next => InstructionLine(None, inst, next)
    }
    def lineWithLabel = (label ~ instruction ~ opt(comment) ~ NEWLINE() ~ opt(line)) ^^ {
      case label ~ inst ~ _ ~ _ ~ next => InstructionLine(Some(label), inst, next)
    }

    lineWithoutLabel | lineWithLabel
  }

  def commentedLine: Parser[Line] = positioned {
    (opt(label) ~ opt(comment) ~ NEWLINE() ~ opt(line)) ^^ {
      case label ~ _ ~ _ ~ next => CommentedLine(label, next)
    }
  }

  def variableLine: Parser[Line] = positioned {
    (label ~ EQUALS() ~ number ~ opt(comment) ~ NEWLINE() ~ opt(line)) ^^ {
      case Label(l) ~ _ ~ NUMBER(n) ~ _ ~ _ ~ next => VariableLine(l, n, next)
    }
  }

  def bytesLine: Parser[Line] = positioned {
    def lineWithoutLabel = (bytesDirective ~ opt(comment) ~ NEWLINE() ~ opt(line)) ^^ {
      case BytesDirective(data) ~ _ ~ _ ~ next => BytesLine(None, data, next)
    }
    def lineWithLabel = (label ~ bytesDirective ~ opt(comment) ~ NEWLINE() ~ opt(line)) ^^ {
      case label ~ BytesDirective(data) ~ _ ~ _ ~ next => BytesLine(Some(label), data, next)
    }

    lineWithoutLabel | lineWithLabel
  }

  def line: Parser[Line] = positioned {
    instructionLine | commentedLine | variableLine | bytesLine
  }

  def origin: Parser[ORIGIN] = positioned {
    (ASTERISK() ~ EQUALS() ~ number ~ opt(comment) ~ NEWLINE()) ^^ {
      case _ ~ _ ~ NUMBER(value) ~ _ ~ _ => ORIGIN(value)
    }
  }

  def section: Parser[Section] = positioned {
    def sectionOrEnd: Parser[Option[Section]] = {
      opt(end | section) ^^ {
        case Some(s) =>
          s match {
            case End()                => None
            case s @ Section(_, _, _) => Some(s)
          }
        case None => None
      }
    }
    (opt(origin) ~ line ~ sectionOrEnd) ^^ {
      case Some(ORIGIN(line)) ~ lineData ~ next => Section(line, lineData, next)
      case _ ~ lineData ~ next                  => Section(0, lineData, next)
    }
  }

  def program: Parser[Section] = positioned {
    phrase(section)
  }
}
