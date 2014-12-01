package nl.henkerikvanderhoek.parser.json

sealed trait JsonValue
case class JsonNull() extends JsonValue
case class JsonBoolean (value: Boolean) extends JsonValue
case class JsonNumber (value: Integer) extends JsonValue
case class JsonArray (value: List[JsonValue]) extends JsonValue
case class JsonObject (value: Map[String,JsonValue]) extends JsonValue
case class JsonString (value: String) extends JsonValue
