package `scala-6502-assembler`
import java.io.File
import scopt.OParser

case class Config(
  in: File = new File(".")
)

object Assembler extends App {
  println("Hello")
  var builder = OParser.builder[Config]
  val parser = {
    OParser.sequence(
      builder.programName("Scala 6502 Assembler"),
      builder.opt[File]('i', "in")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(in = x))
        .text("in is a required file property")
    )
  }
  OParser.parse(parser, args, Config()) match {
    case Some(config) => {
    }
    case _ => {
      // oh no
    }
  }
}
