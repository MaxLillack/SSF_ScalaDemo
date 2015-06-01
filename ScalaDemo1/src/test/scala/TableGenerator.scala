
import org.scalatest._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.io._

object Data {
	val xml = 
	<TableScript>
		<Table name="Project">
			<ForeignKey fk="Repository" refTable="Repository"/>
			<ForeignKey fk="Person" refTable="Person"/>
			<Attribute name="Name" type="string" />
		</Table>
		<Table name="Person">
			<!-- SIC -->
			<ForeignKey fk="Person" refTable="Person"/>
			<ForeignKey fk="Person" refTable="Person"/>
			<Attribute name="Name" type="string" />
			<Attribute name="ProjectRole" type="string" />
		</Table>
		<Table name="WorkItem">
			<ForeignKey fk="Project" refTable="Project"/>
			<Attribute name="Opened" type="datetime" />
			<Attribute name="Closed" type="datetime" />
			<Attribute name="Description" type="string" />
		</Table>
		<Table name="FileAttachment">
			<ForeignKey fk="WorkItem" refTable="WorkItem"/>
			<Attribute name="Path" type="string" />
		</Table>
	</TableScript>
}


case class Table(name: String, attributes: Seq[Attribute])
{
	override def toString() =
		s"""CREATE TABLE $name (
		    ${name}_ID NUMBER(10) NOT NULL,
			${attributes mkString(",\n")}
			CONSTRAINT ${name}_PK PRIMARY KEY (${name}_ID)
		);"""
}
case class Attribute(name: String, typeName: String)
{
	def typeStr = typeName match {
		case "int" => "number(10)"
		case "string" => "varchar2(400)"
		case "datetime" => "DATE"
	}
	override def toString() = s"$name $typeStr"
}

class TableGeneratorTest extends FunSuite {
  test("test1") {
	val xml = Data.xml
	val tables = (xml \ "Table").map { table =>
		val attributes = (table \ "Attribute").map {
			attribute => Attribute((attribute \ "@name").text,
								   (attribute \ "@type").text)
		}
		Table((table \ "@name").text, attributes)
	}
	println(tables mkString("\n"))
  }

}