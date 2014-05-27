import org.scalatest.FunSuite

class MacroTest extends FunSuite {
  test("macrotest") {

	val name = "FuturisticCar"
	val car = Demo.CarGenerator.generateCar(name, Seq(
		Demo.CarConfig.CarBody, 
		Demo.CarConfig.AutomaticTransmission, 
		Demo.CarConfig.GasolineEngine, 
		Demo.CarConfig.ElectricEngine 
		//Demo.CarConfig.TrailerCoupling
		))
	
	car.sayName
	
	car.carbody
	car.transmission
	car.engine
	//car.trailerCoupling
  }
}
