package nl.henkerikvanderhoek.parser.json

import nl.henkerikvanderhoek.parser.json.JsonParser.pJsonValue
import org.specs2.mutable.Specification

class JsonSpec extends Specification {
  val arrayOfIntegers = JsonArray(List(JsonNumber(1),JsonNumber(2),JsonNumber(3)))

  def run(inp:String):JsonValue =
    pJsonValue.run(inp).head._1

  "The JSON Parser" should {
    "parse a number" in
      { run("22") === JsonNumber(22) }

    "parse a string" in
      { run("""  "test"  """) === JsonString("test")}

    "parse true" in
      { run("true") === JsonBoolean(true) }

    "parse false" in
      { run("false") === JsonBoolean(false) }

    "parse null" in
      { run("null") === JsonNull() }

    "parse an array of numbers" in
      { run("[1,2,3]") === arrayOfIntegers }

    "parse an array of booleans" in
      { run("[true,false,false]") === JsonArray(List(JsonBoolean(true),JsonBoolean(false),JsonBoolean(false))) }

    "parse an simple object" in
      { run("""{ "x": 1, "y": false }""") === JsonObject(Map("x" -> JsonNumber(1), "y" -> JsonBoolean(false)))}

    "parse an more complex object" in
      { run("""{ "a": [1,2,3], "b": { "c": true } }""") === JsonObject(Map(
          "a" -> arrayOfIntegers,
          "b" -> JsonObject(
             Map("c" -> JsonBoolean(true))
          )
      )) }
  }
}
