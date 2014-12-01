package nl.henkerikvanderhoek.parser.json

import nl.henkerikvanderhoek.parser.Parser
import nl.henkerikvanderhoek.parser.{ Parser => P }

object JsonParser {
  val pJsonNull:Parser[JsonNull] =
    P.pSym("null").as(JsonNull())

  val pJsonBoolean:Parser[JsonBoolean] = {
    val pTrue  = P.pSym("true").as(JsonBoolean(true))
    val pFalse = P.pSym("false").as(JsonBoolean(false))
    pTrue choice pFalse
  }

  val pJsonString:Parser[JsonString] =
    for { str <- P.pToken(P.pString) } yield JsonString(str)

  val pJsonNumber:Parser[JsonNumber] =
    for { n <- P.pToken(P.pDigits) } yield JsonNumber(n)

  val pJsonArray:Parser[JsonArray] =
    for { xs <- P.pBrackets(P.pSepBy(pJsonValue, P.pComma)) } yield JsonArray(xs)

  val pJsonObject:Parser[JsonObject] = {
    val pPair = for {
      k <- P.pString
      _ <- P.pColon
      v <- pJsonValue
    } yield (k, v)

    for { xs <- P.pBraces(P.pSepBy(pPair, P.pComma)) } yield JsonObject(xs.toMap)
  }

  val pJsonValue:Parser[JsonValue] =
    pJsonObject.choice(pJsonNumber)
               .choice(pJsonArray)
               .choice(pJsonBoolean)
               .choice(pJsonNull)
               .choice(pJsonString)
}