import org.scalatest.{FlatSpec, DiagrammedAssertions}
import scala_6502_assembler.{AssemblerParser, Comment}


class ParserSpec extends FlatSpec with DiagrammedAssertions {
  behavior of "String Parsing"

  it should "passthrough strings" in {
    val parser = new AssemblerParser
    val result = parser.parse(parser.word, "Hi")
    assert { result.successful }
    assert { result.get == "Hi" }
  }

  behavior of "Comment Parsing"

  it should "correctly parse comments" in {
    val parser = new AssemblerParser
    val result = parser.parse(parser.comment, "; Here is a comment")
    assert { result.successful }
    val comment = result.get match {
      case Comment(string) => Some(string)
      case _ => None
    }
    assert { comment.get == "Here is a comment" }
  }
}
