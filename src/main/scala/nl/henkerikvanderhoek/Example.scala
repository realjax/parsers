package nl.henkerikvanderhoek

import nl.henkerikvanderhoek.parser.json.{JsonParser => P}

object Example {
  def main(arr:Array[String]):Unit = {
    val inp = """{ "x": [1,2,3], "y": { "z": true } }"""
    println(P.pJsonObject.run(inp))
  }
}
