package scala_6502_assembler
import java.io.{File, FileOutputStream}
import scala.io.Source
import scopt.OParser
import scala_6502_assembler.error.AssemblerCompilationError
import scala_6502_assembler.lexer.AssemblerLexer
import scala_6502_assembler.parser.{AssemblerParser, Section}

case class Config(
    in: File = new File(".")
)

object Main extends App {
  var builder = OParser.builder[Config]
  val parser = {
    OParser.sequence(
      builder.programName("Scala 6502 Assembler"),
      builder
        .opt[File]('i', "in")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(in = x))
        .text("in is a required file property")
    )
  }
  OParser.parse(parser, args, Config()) match {
    case Some(config) => {
      val source = Source.fromFile(config.in)
      val data = try {
        source.mkString
      } finally {
        source.close()
      }
      println(AssemblerLexer(data).right)
      val assembly = AssemblerCompiler(data)
      var out = new FileOutputStream("./out.xex")
      var map = LabelResolver.getLabelMap(assembly.right.get)
      AssemblerCompiler
        .toXex(assembly.right.get, map)
        .map(_.toByte)
        .foreach(out.write(_))
      out.close()
      println(AssemblerCompiler.assemble(assembly.right.get, map))
    }
    case _ => {
      // oh no
    }
  }
}

object AssemblerCompiler {
  def apply(
      code: String
  ): Either[AssemblerCompilationError, Section] = {
    for {
      tokens <- AssemblerLexer(code).right
      ast <- AssemblerParser(tokens).right
    } yield ast
  }

  def assemble(code: Section, map: LabelResolver.LabelMap): String = {
    code.toVirtual6502(map)
  }

  def toXex(code: Section, map: LabelResolver.LabelMap): List[Int] = {
    code.toXex(map)
  }
}
