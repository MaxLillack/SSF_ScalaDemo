import org.scalatest.FunSuite

class MacroDemoTest extends FunSuite {
  test("demo1") {
	Demo.MacroDemo.demo1("Max")	
  }
  test("demo2") {
    val x = 5
	Demo.MacroDemo.demo2(x > 1)
  }
  test("demo3") {
    val x = 5
	Demo.MacroDemo.printf("hello %s %d (%s)\n", "world", 123, "!")
  }
}
