package scala_6502_assembler
import java.io.File
import scala.io.Source
import scopt.OParser
import scala_6502_assembler.error.AssemblerCompilationError
import scala_6502_assembler.lexer.AssemblerLexer
import scala_6502_assembler.parser.{AssemblerParser, AssemblerAST}

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
      println(AssemblerCompiler.assemble(AssemblerCompiler(data).right.get))
    }
    case _ => {
      // oh no
    }
  }
}

object AssemblerCompiler {
  def apply(
      code: String
  ): Either[AssemblerCompilationError, List[AssemblerAST]] = {
    for {
      tokens <- AssemblerLexer(code).right
      ast <- AssemblerParser(tokens).right
    } yield ast
  }

  def assemble(code: List[AssemblerAST]): List[Integer] = {
    code.map(_.toBytes).flatten
  }
}
