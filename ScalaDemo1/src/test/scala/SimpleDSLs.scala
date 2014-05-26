import org.scalatest._

case class Amount(amount: Int, stock: String)
trait Action {	def zu(price: Int) = new Trade(this, price) }
case class BuyAction(amount: Amount) extends Action
case class SellAction(amount: Amount) extends Action
case class Trade(action: Action, price: Int)

object DSLImplicits {
	implicit def toAmount(amount: Int) = new AmountBuilder(amount)
	class AmountBuilder(amount: Int) {
		def IBM() : Amount = new Amount(amount, "IBM")
	}
	def kaufe(amount: Amount) = BuyAction(amount)
	def verkaufe(amount: Amount) = SellAction(amount)
}

class SimpleDSLTests extends FunSuite {
  test("1") {
	import DSLImplicits._
	
	println(kaufe (100 IBM) zu 12)
	println(verkaufe (50 IBM) zu 11)
  }

}