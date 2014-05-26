
import org.scalatest._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.io._

object DepotDSL extends JavaTokenParsers {
  def model = (aktien | depot).?
  def aktien = "Aktien" ~ "{" ~> aktie.* <~ "}"
  def depot = ("Depot" ~> ident) ~ ("Startkapital" ~> decimalNumber) ~ ("{" ~> rep(transaktion)) <~ "}" ^^ {
    case name ~ kapital ~ transaktionen => Depot(name, kapital, transaktionen)
  }

  def transaktion = (kauf | verkauf)
  def aktie = (ident ~ stringLiteral <~ ":") ~ ident <~ ";" ^^ {
    case name ~ anzeigeName ~ kategorie => Aktie(name, anzeigeName, kategorie)
  }
  def kauf = ("kaufe" ~> wholeNumber ~ ident) ~ ("zu" ~> decimalNumber) <~ ";" ^^ {
    case anzahl ~ aktie ~ preis => Kauf(anzahl, aktie, preis)
  }
  def verkauf = ("verkaufe" ~> wholeNumber ~ ident) ~ ("zu" ~> decimalNumber) <~ ";" ^^ {
    case anzahl ~ aktie ~ preis => Verkauf(anzahl, aktie, preis)
  }
}

case class Aktie(name: String, anzeigeName: String, kategorie: String)
case class Depot(name: String, kapital: String, trans: Seq[Transaktion])

trait Transaktion {
  def aktie: String
  def anzahl: String
  def preis: String
}

case class Kauf(anzahl: String, aktie: String, preis: String) extends Transaktion
case class Verkauf(anzahl: String, aktie: String, preis: String) extends Transaktion

object Interpreter {

  def apply(depot : Depot) {

    var bar = depot.kapital.toDouble

    depot.trans.foreach(t => t match {
      case Kauf(anzahl, aktie, preis) => bar -= preis.toDouble * anzahl.toInt
      case Verkauf(anzahl, aktie, preis) => bar += preis.toDouble * anzahl.toInt
    })

    val gesamtVolumen = depot.trans.foldLeft(0.0)((current: Double, trans: Transaktion) => trans match {
      case Kauf(anzahl, aktie, preis) => current + preis.toDouble * anzahl.toInt
      case Verkauf(anzahl, aktie, preis) => current - preis.toDouble * anzahl.toInt
    })

    val groupedAktien = depot.trans.groupBy(_.aktie)
    val volumeOfAktie = groupedAktien.map {
      case (aktie, transaction) => (aktie, transaction.foldLeft(0.0)((current: Double, trans: Transaktion) => trans match {
        case Kauf(anzahl, aktie, preis) => current + preis.toDouble * anzahl.toInt
        case Verkauf(anzahl, aktie, preis) => current - preis.toDouble * anzahl.toInt
      }))
    }
	    
	val countOfAktie = groupedAktien.map {
      case (aktie, transaction) => (aktie, transaction.foldLeft(0.0)((current: Double, trans: Transaktion) => trans match {
        case Kauf(anzahl, aktie, preis) => current + anzahl.toInt
        case Verkauf(anzahl, aktie, preis) => current - anzahl.toInt
      }))
    }
	
	/*
    println("volumeOfAktie")
    println(volumeOfAktie)
	
	println("countOfAktie")
    println(countOfAktie)
	*/
	
    val nonZeroAktien = volumeOfAktie.filter {case (aktie, vol) => vol != 0.0}

    val report = s"Depot ${depot.name} \n" +
                 s"Barbestand: ${bar} \n" +
                 s"Gesamtvolumen: ${gesamtVolumen} \n" +
                 s"Aktien:\n"+
                 s"    ${nonZeroAktien.map{case (aktie: String, vol: Double) => s"${aktie}: ${countOfAktie(aktie)} (${vol})"}.mkString("\n")}"

    println(report)
  }
}


class DepotTests  extends FunSuite {
  test("1") {
    println("test")
  }

  test("parse1") {
    val test1 =
      """Aktien{
          WKN_1 "Sony": technologie;
          WKN_2 "Deutsche Bank": bank;
        }""".stripMargin

    DepotDSL.parseAll(DepotDSL.model, test1) match {
      case DepotDSL.Success(lup,_) => println(lup)
      case x => println(x)
    }
  }

  test("parse2") {
    val depot = """			Depot Alex Startkapital 1000.00{
                  				kaufe 100 WKN_1 zu 2.50;
                  				kaufe 100 WKN_2 zu 1.50;

                  				verkaufe 50 WKN_1 zu 2.75;
                  				verkaufe 50 WKN_1 zu 2.25;
                  				verkaufe 50 WKN_2 zu 2.50;
                          verkaufe 50 WKN_2 zu 0.50;
                  			}"""

    DepotDSL.parseAll(DepotDSL.model, depot) match {
      case DepotDSL.Success(lup,_) => println(lup)
      case x => println(x)
    }
  }

  test("report") {
    val depot = """			Depot Alex Startkapital 1000.00{
                  				kaufe 100 WKN_1 zu 2.50;
                  				kaufe 100 WKN_2 zu 1.50;

                  				verkaufe 50 WKN_1 zu 2.75;
                  				verkaufe 50 WKN_1 zu 2.25;
                  				verkaufe 50 WKN_2 zu 2.50;

                  			}"""



    DepotDSL.parseAll(DepotDSL.depot, depot) match {
      case DepotDSL.Success(lup,_) => Interpreter(lup)
      case x => println(x)
    }
  }
}