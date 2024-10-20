import scala.collection.immutable.ListSet

@main def hello(): Unit =
  val a = ListSet("a", "b", "c")
  val b = ListSet[String]("b")
  println((a & b).isEmpty)

def msg = "I was compiled by Scala 3. :)"
