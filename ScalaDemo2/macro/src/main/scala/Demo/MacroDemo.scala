package Demo

import scala.reflect.macros.Context
import scala.collection.mutable.{ListBuffer, Stack}
import scala.language.experimental.macros

object MacroDemo {
  def demo1(name: String): Any = macro demo1_impl
  def demo1_impl(c: Context)(name: c.Expr[String]) = {
    import c.universe._
	val tree = q"println($name)"

	// println(showRaw(tree))
	// Apply(Ident(TermName("println")), List(Literal(Constant("Max"))))
	
	// println(show(tree))
	// println("Max")
	
	tree
  }
  
  def demo2(condition: Boolean): Any = macro demo2_impl
  def demo2_impl(c: Context)(condition: c.Expr[Boolean]) = {
    import c.universe._
	
	// Make code from tree
	val aCode = show(condition.tree)
	
	// Condition will be evaluated at runtime
	val tree = q"""println($aCode + " = " + $condition)"""
	println(showCode(tree))
	tree
  }
  
  // Example based on http://docs.scala-lang.org/overviews/macros/overview.html
  def printf(format: String, params: Any*): Unit = macro printf_impl
  def printf_impl(c: Context)(format: c.Tree, params: c.Tree*) = {
    import c.universe._
	
	val q"${value: String}" = format
	
    val paramsStack = Stack[Tree]() ++ params
    val refs = value.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => q"${paramsStack.pop}"
      case "%s" => q"${paramsStack.pop}"
      case "%%" => q"%"
      case part => q"$part"
    }

    val stats = refs.map(ref => q"print($ref)")
    q"{ ..$stats }"
  }
}


