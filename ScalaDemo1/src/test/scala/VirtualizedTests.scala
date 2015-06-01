import org.scalatest._

class VirtualizedTests extends FunSuite {
  test("1") {
	  def __ifThenElse[T](cond: => Boolean,
						  thenp: => T, elsep: => T): T = {
		println("if: "+cond)
		thenp
		}

	  val x0 = if(false) "Foo" else "Bar" // virtualized to: ‘__ifThenElse(false, 1, 2)‘
	  println(x0) // if:false Foo
  }
}