// Based on https://github.com/scala-lms/tutorials

import org.scalatest._
import scala.lms.common._
import scala.reflect.SourceContext


class LMSTests extends FunSuite {
  test("1") {
    val snippet = new DslDriver[Int,Int] {
      //#1
      def snippet(x: Rep[Int]) = {
        def compute(b: Boolean): Rep[Int] = {
          // the if is executed in the first stage
          if (b) 1 else x
        }
        compute(true)+compute(1==1)
      }
      //#1
    }
    /*
    val snippet = new DslDriver[Int,Int] {
      //#1
      def snippet(x: Rep[Int]) = {
        def compute(b: Boolean): Rep[Int] = {
          // the if is executed in the first stage
          if (b) 1 else x
        }
        compute(true)+compute(1==1)
      }
      //#1
     */
    val test2 = new DslDriver[Int,Int] {
      //#1
      def snippet(x: Rep[Int]) = {
        def compute(b: Rep[Boolean]): Rep[Int] = {
          if (b) 1 else x
        }
        compute(true) + compute(x == 1)
      }
      //#1
    }

    val example = (x: Int) => {
        def compute(b: Boolean): Int = {
          if (b) 1 else x
        }
        compute(true) + compute(x == 1)
    }
    println("example: " + example(0))

    println("snippet 1")
    println(snippet.code)
    println("test2")
    println(test2.code)

    assert(snippet.eval(0) === 2)
  }

  test("2") {
    val snippet = new  DslDriver[Int,Int] {
      //#2
      def snippet(x: Rep[Int]) = {
        def compute(b: Rep[Boolean]): Rep[Int] = {
          // the if is deferred to the second stage
          if (b) 1 else x
        }
        compute(x==1)
      }
      //#2
    }
    assert(snippet.eval(2) === 2)
  }

  test("power") {
    val snippet = new DslDriver[Int,Int] {
      //#power
      def power(b: Rep[Int], x: Int): Rep[Int] = {
        if (x == 0) 1
        else b * power(b, x-1)
      }
      def snippet(b: Rep[Int]) =  power(b, 3)
      //#power
    }
	println("power")
    assert(snippet.eval(2) === 8)
    println(snippet.code)
  }

  test("power1") {
    val snippet = new DslDriver[Int,Int] {
      //#power
      def power(b: Rep[Int], x: Int): Rep[Int] = {
        if (x == 0) 1
        else b * power(b, x-1)
      }
      def snippet(b: Rep[Int]) =  power(b, 1)
      //#power
    }
    assert(snippet.eval(2) === 2)
    println(snippet.code)
  }

  test("range1") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = {
        comment("for", verbose = false) {
          //#range1
          for (i <- (0 until 3): Range) {
            println(i)
          }
          //#range1
        }
      }
    }
    println(snippet.code)
  }

  test("range2") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = {
        comment("for", verbose = false) {
          //#range2
          for (i <- (0 until x): Rep[Range]) {
            println(i)
          }
          //#range2
        }
      }
    }
    println(snippet.code)
  }

  test("matrix") {
    val snippet = new DslDriver[Array[Int],Array[Int]] {
      //#unrollIf
      def unrollIf(range: Range)(cond: Boolean) = new {
        def foreach(f: Rep[Int] => Rep[Unit]): Rep[Unit] = {
          if (cond) for (i <- range) f(i)
          else for (i <- (range.start until range.end): Rep[Range]) f(i)
        }
      }
      //#unrollIf
      val A = scala.Array

      val a =
      //#ex-a
        A(A(1, 1, 1, 1, 1), // dense
          A(0, 0, 0, 0, 0), // null
          A(0, 0, 1, 0, 0), // sparse
          A(0, 0, 0, 0, 0),
          A(0, 0, 1, 0, 1))
      //#ex-a

      def infix_at(a: Array[Array[Int]], i: Int, j: Rep[Int]): Rep[Int] = {
        (staticData(a(i)) apply j)
      }

      def sparse(ai: Array[Int]): Boolean = {
        ai.filter(_ != 0).length < 3
      }

      //#matrix_vector_prod
      def matrix_vector_prod(a: Array[Array[Int]], v: Rep[Array[Int]]) = {
        val n = a.length
        val v1 = NewArray[Int](n)
        for (i <- 0 until n: Range) {
          comment("row_" + i) {
            for (j <- unrollIf(0 until n)(sparse(a(i)))) {
              v1(i) = v1(i) + a.at(i, j) * v(j)
            }
          }
        }
        v1
      }
      //#matrix_vector_prod

      def snippet(v: Rep[Array[Int]]) = {
        matrix_vector_prod(a, v)
      }
    }

    println(snippet.code)
	val v = scala.Array(5, 4, 3, 2, 1)
	println(snippet.eval(v).deep.mkString(" "))
  }
}