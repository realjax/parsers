package nl.henkerikvanderhoek.parser

case class Parser[+A](parse: String => List[(A,String)]) {
  def flatMap[B](f: A => Parser[B]):Parser[B] =
    Parser { inp => parse(inp).map { case (a,rem) => f(a).parse(rem) }.flatten }

  def sequence[B](p: => Parser[B]):Parser[B] =
    flatMap { Function.const(p) }

  def map[B](f: A => B):Parser[B] =
    Parser { inp => parse(inp).map { case (a,rem) => (f(a),rem) } }

  def as[B](b: B):Parser[B] =
    map { Function.const(b) }

  def choice[B >: A](p: => Parser[B]):Parser[B] =
    Parser { inp => parse(inp) ++ p.parse(inp) }

  def run(inp: => String):List[(A,String)] =
    Parser.pWhitespace.sequence(this).parse(inp)
}

object Parser {
  def apply[A](a: => A): Parser[A] =
    Parser { inp => List((a,inp)) }

  def pSatisfy(p:Char => Boolean):Parser[Char] =
    Parser { inp => !inp.isEmpty && p(inp.head) match {
      case true  => List((inp.head,inp.tail))
      case false => Nil
    }}

  def pChar(c:Char):Parser[Char] = pSatisfy { x => x == c }

  def pOneOrMore[A](p: => Parser[A]):Parser[List[A]] = for {
    a  <- p
    as <- pZeroOrMore(p)
  } yield a::as

  def pZeroOrMore[A](p: => Parser[A]):Parser[List[A]] =
    pOneOrMore(p).choice(Parser(Nil))

  def pWhitespace = pZeroOrMore(pSatisfy { c => ' ' == c  })

  def pToken[A](p: => Parser[A]):Parser[A] = for {
    a <- p
    _ <- pWhitespace
  } yield a

  def pSepBy1[A,B](p: => Parser[A], q: => Parser[B]):Parser[List[A]] = for {
    a  <- p
    as <- pZeroOrMore(q.sequence(p))
  } yield a::as

  def pSepBy[A,B](p: => Parser[A], q: => Parser[B]):Parser[List[A]] =
    pSepBy1(p, q).choice(Parser(Nil))

  def pSym(s:String):Parser[String] = {
    def go(xs:String):Parser[String] =
      xs.isEmpty match {
        case true  => Parser("")
        case false => for {
          a  <- pChar(xs.head)
          as <- go(xs.tail)
        } yield a + as
      }

    pToken(go(s))
  }

  def pEnclose[L,A,R](l: => Parser[L], p: => Parser[A], r: => Parser[R]) =
    for { _ <- l; a <- p; _ <- r } yield a

  def pColon    = pSym(":")
  def pComma    = pSym(",")
  def pBracketL = pSym("[")
  def pBracketR = pSym("]")
  def pBracesL  = pSym("{")
  def pBracesR  = pSym("}")
  def pQuote    = pSym("\"")

  def pBrackets[A](p: => Parser[A]):Parser[A] =
    pEnclose(pBracketL, p, pBracketR)

  def pBraces[A](p: => Parser[A]):Parser[A] =
    pEnclose(pBracesL, p, pBracesR)

  def pQuotes[A](p: => Parser[A]):Parser[A] =
    pEnclose(pQuote, p, pQuote)

  def pString:Parser[String] =
    pQuotes(pOneOrMore(pSatisfy(c => ('a' to 'z').contains(c)))).map { xs => xs.mkString }

  def pDigit:Parser[Integer] =
    pSatisfy { c => ('0' to '9').contains(c) }.map { c => c.asDigit }

  def pDigits: Parser[Integer] =
    pOneOrMore(pDigit).map { xs =>
      xs.reverse.zipWithIndex.map { case (n,b) => n * scala.math.pow(10,b).intValue }.sum
    }
}