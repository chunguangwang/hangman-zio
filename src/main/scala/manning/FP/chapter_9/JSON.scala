package manning.FP.chapter_9

/*
We’ll write a rather dumb parser that simply parses a syntax tree from the document
without doing any further processing.11 We’ll need a representation for a parsed JSON document.
Let’s introduce a data type for this:
 */
trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

