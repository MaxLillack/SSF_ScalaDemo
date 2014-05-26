
import org.scalatest._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.io._


object Controls {
  def unless(condition: => Boolean)(block: => Unit) = {
    if(!condition) block
  }
}

class UnlessTest extends FunSuite {
  test("1") {
	import Controls._
	val x = 1
    unless(x > 0) {
		println("unless 1")
	}
	
	unless(x < 0) {
		println("unless 2")
	}
	
	// is the same as
	
	val temp = unless(x < 0) _
	temp(println("unless 2"))
	
  }

}