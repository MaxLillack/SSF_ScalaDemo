package Demo

import scala.reflect.macros.Context
import scala.collection.mutable.{ListBuffer, Stack}
import scala.language.experimental.macros


object CarConfig extends Enumeration {
	type CarConfig = Value
	val CarBody = Value
	val AutomaticTransmission = Value
	val ManualTransmission = Value
	val ElectricEngine = Value
	val GasolineEngine = Value
	val TrailerCoupling = Value
}


object CarGenerator {
  import CarConfig._

  def generateCar(name: String, config:Seq[CarConfig]): Any = macro generateCar_impl
  def generateCar_impl(c: Context)(name: c.Tree, config: c.Tree) = {
    import c.universe._
	
	// Create new name for class
    val className = newTypeName("Car")

	// load configuration from parameter
	val features = config match {
		case q"$seq(...$exprss)" => exprss.flatten
	}
	
	// helper function for config lookup
	def featureExists(name: String) = features.exists(f => f match { case q"$expr.$foundName" => foundName == TermName(name)})
	
	// Check Configuration
	if(!featureExists("CarBody")) {
		c.abort(c.enclosingPosition, "Missing mandatory feature CarBody")
	}
	
	if(featureExists("AutomaticTransmission") && featureExists("ManualTransmission")) {
		c.abort(c.enclosingPosition, "Select only one transmission type")
	}
	
	if(!featureExists("ElectricEngine") && !featureExists("GasolineEngine")) {
		c.abort(c.enclosingPosition, "You need an engine!")
	}
	
	if(featureExists("ElectricEngine") && !featureExists("AutomaticTransmission")) {
		c.abort(c.enclosingPosition, "Electric engine requires automatic transmission")
	}
	
	// Build Car
	var methods = ListBuffer[DefDef]()
	
	if(featureExists("CarBody")) {
		val carbody = q"""def carbody = println("Hello from CarBody")"""
		methods = methods :+ carbody
	}
	
	def selectTransmission(): DefDef = { 
		if(featureExists("AutomaticTransmission")) {
			return q"""def transmission = println("Hello from AutomaticTransmission")"""
		}
		if(featureExists("ManualTransmission")) {
			return q"""def transmission = println("Hello from ManualTransmission")"""
		}
		null
	}
	
	methods = methods :+ selectTransmission
	
	var engines = ListBuffer[Tree]()

	if(featureExists("ElectricEngine")) {
		val electricEngine = q"""println("Engine: Piep Piep")"""
		engines = engines :+ electricEngine
	}
	if(featureExists("GasolineEngine")) {
		val gasEngine = q"""println("Engine: Brumm Brumm")"""
		engines = engines :+ gasEngine
	}
	
	val engine = q"""def engine = { ..$engines }"""
	methods = methods :+ engine
	
	if(featureExists("TrailerCoupling")) {
		val trailerCoupling = q"""def trailerCoupling = println("Hello from TrailerCoupling")"""
		methods = methods :+ trailerCoupling
	}
	
	println(s"Inserting ${methods.length} methods")
	
	// Put everything together
	val carClass = q"""
    class $className(name: String)  {
	  def sayName = println("Name: " + $name)
	  
	  ..$methods
    }
	new $className($name)
    """
	
	//println("Generated tree")
	//println(showRaw(carClass))
	
	println("Generated source code")
	println(show(carClass))
	
	// return generated code
	carClass
  }

}


