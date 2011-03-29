import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.Iterator
import scala.collection.immutable.List
import scala.collection.mutable.HashSet
import scala.collection.immutable.{HashSet => ImmutableHashSet}
import scala.collection.mutable.HashMap
import scala.collection.immutable.{HashMap => ImmutableHashMap}

case class Predicate(val name : String) extends String {}
case class Value(val v : Int) extends Int {}
case class Fact(predicate : Predicate, values : List[Value]) {
	override def toString =
		predicate + "[" + values.reduceLeft(_ + ", " + _) + "]"
}

case class Variable(val name : String, val annotated : Boolean = false) extends String {
	override def toString =
		(if(annotated) "@" else "") + super.toString
}
case class Query(predicate : Predicate, variables : List[Variable]) {
	override def toString =
		predicate + "[" + variables.reduceLeft(_ + ", " + _) + "]"
}

class XDB(val hash : Set[Fact]) extends Set[Fact] {
	def this = this(new HashSet[Fact])
	def hasFact(fact : Fact) : Boolean = hash containsEntry fact
	def addFact(fact : Fact) : Boolean = hash addEntry fact
	def getValuesForPredicate(p : Predicate) : HashSet[List[Value]] =
		hash filter { _.predicate == p } map { _.values }
}

var nextNodeId = 0

def generateNode(id : Value) : Actor = {
	var idb = new XDB
	actor { loop { receive {
		case (sender: Actor, timestamp: List[Int], 'store, fact : Fact) =>
			//update timestamp
			sender ! ((self, timestamp, idb addFact fact))
		case (sender: Actor, timestamp: List[Int], 'bindings, query : Query) =>
			val argLists = idb getValuesForPredicate query.predicate
			val mapSet = for(args <- argLists) yield query.variables zip args toMap
			//update timestamp
			sender ! ((self, timestamp, mapSet))
		case (sender: Actor, timestamp: List[Int], 'idb) =>
			sender ! ((self, timestamp, idb))
	}}}
}


case class Rule(val head : Query, val body : Set[Query]) {}

// TODO: take input from user here
// -- begin static input--
val rules =
	new List[Rule](
		Rule(
			Query(Predicate "P", List(Variable("b", true), Variable "c")),
			List(
				Query(Predicate "Q", List(Variable "b", Variable("c", true)))
			)
		),
		Rule(
			Query(Predicate "Q", List(Variable("b", true), Variable "c")),
			List(
				Query(Predicate "R", List(Variable("b", true))),
				Query(Predicate "S", List(Variable("c", true)))
			)
		)
	)
val edb =
	new XDB(
		Set[Fact](
			Fact(Predicate "R", List(Value 0)),
			Fact(Predicate "S", List(Value 1))
		)
	)
// -- end static input--


var domain = new HashSet[Value]
var nodes = new Set[Actor]

var changed = true
while(changed) {
	rules foreach { (rule) =>
		rule.body foreach { (query) =>
			// interesting stuff goes here
		}
	}
}

// for later:
//var a = List((1,2),(2,3),(3,4)) toMap
//List(1,3).foldLeft(a) (_ - _)
