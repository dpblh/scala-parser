package parser

import scala.util.matching.Regex


/**
 * Парсер использует строку или регулярку в качестве атома
 */
trait RegexpParsers extends Parsers {

  def Reader(s: String) = new RegexReader(s)

  /**
   * Сохранаем пробельный символы
   * @param r Regex | s String
   * @return
   */
  def saveSpace(r: Regex): Parser[String] = reg2parser(r, saveSpace = true)
  def saveSpace(s: String): Parser[String] = srt2parser(s, saveSpace = true)

  /**
   * Имплисит конвертации для строк и регулярок
   * @param r Regex | s String
   * @return
   */
  implicit def regex2parser(r: Regex): Parser[String] = reg2parser(r, saveSpace = false)
  implicit def string2parser(s: String): Parser[String] = srt2parser(s, saveSpace = false)

  def srt2parser(s: String, saveSpace: Boolean): Parser[String] = Parser { in =>
    val source = in.source
    val start = if (saveSpace) in.offset else in.skipWhiteSpace.offset
    //    TODO не оптимально создавать строку на каждый проход
    source.substring(start, source.length).indexOf(s) match {
      case 0 => Success(source.subSequence(start, start + s.length).toString, in.skip(start + s.length))
      case _ => Failed(s"Не найден $s в ${source.subSequence(start, source.length)}")
    }
  }

  def reg2parser(r: Regex, saveSpace: Boolean): Parser[String] = Parser { in =>
    val source = in.source
    val start = if (saveSpace) in.offset else in.skipWhiteSpace.offset
    r.findPrefixMatchOf(source.subSequence(start, source.length)) match {
      case Some(x) => Success(source.subSequence(start, start + x.end).toString, in.skip(start + x.end))
      case _ => Failed(s"Не найден $r в ${source.subSequence(start, source.length)}")
    }
  }

}