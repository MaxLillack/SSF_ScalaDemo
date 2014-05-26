
import org.scalatest._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.io._

object MyDSL extends JavaTokenParsers {
  def hello = ("Hello" | "Hi") ~ ident ^^ { case hello ~ name => println("Hallo " + name) }
}

class ParserTests  extends FunSuite {

  test("parse1") {
    val test = """Hello Max"""

    MyDSL.parseAll(MyDSL.hello, test) match {
      case MyDSL.Success(lup,_) => println("Parsing done")
      case x => println(x)
    }
  }
  
  test("parse2") {
    val test = """Hi Max"""

    MyDSL.parseAll(MyDSL.hello, test) match {
      case MyDSL.Success(lup,_) => println("Parsing done")
      case x => println(x)
    }
  }

}