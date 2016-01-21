package parser

/**
 * Created by tim on 14.12.15.
 */
trait CharsParsers extends Parsers {

  def Reader(s: String) = new CharsReader(s)

  implicit def string2parser(s: String): Parser[String] = {
    s.foldLeft(success(""))((a, b) => a ~ char(b) fn { case a2 ~ b2 => s"$a2$b2" })
  }

  def char(c: Char): Parser[Char] = char(e => e == c)

  def char(p: Char => Boolean): Parser[Char] = Parser { in =>
    if (in.hasNext && p(in.peek)) Success(in.peek, in.next()) else Failed()
  }

}
