package parser

/**
 * Добавить сообщения об ошибках
 */
sealed trait ParseResult[+U]
case class Success[+U](result: U, reader: Reader) extends ParseResult[U]
case class Failed[+U](error: String = "не известная ошибка") extends ParseResult[U]


trait Parsers {

  def Reader(s: String): Reader

  case class ~[+A,+B](_1: A, _2: B)

  def Parser[T](f: Reader => ParseResult[T]):Parser[T] = new Parser[T] {
    override def apply(in: Reader): ParseResult[T] = f(in)
  }

  /**
   * Возвращает успешный парсер с предустановленным результатом
   * @param result
   * @tparam R
   * @return
   */
  def success[R](result: R) = Parser { in =>
    Success(result, in)
  }

  def parseAll[T](p: Parser[T], s: String):ParseResult[T] = {
    p(Reader(s))
  }

  /**
   * Парсинг входной строки по парсеру
   * @param p парсер
   * @param s строка
   * @tparam T
   * @return
   */
  def parse[T](p: Parser[T], s: String):T = {
    p(Reader(s)) match {
      case Success(result, reader) =>
        val in = reader.skipWhiteSpace
        if (!in.hasNext) result
        else throw new ParseException(s"Ошибка парсинга. Неожиданный конец строки: ${in.source.subSequence(in.offset, in.source.length)}")
      case Failed(msg) => throw new ParseException(s"Ошибка парсинга: $msg")
    }
  }

  /**
   * Ошибка парсинга
   * @param msg сообщение об ошибки
   */
  class ParseException(msg: String) extends RuntimeException(msg)


  abstract class Parser[+T] { self =>

    def apply(in: Reader): ParseResult[T]

    /**
     * Логическое объединение парсеров
     * @param p правый парсер
     * @tparam U тип правового аргумента
     * @return
     */
    def ~[U] (p: => Parser[U]): Parser[~[T,U]] = Parser { in =>
      self.apply(in) match {
        case Success(resultL, readerL) => p(readerL) match {
          case Success(resultR, readerR) => Success(new ~(resultL, resultR), readerR)
          case Failed(msg) => Failed(msg)
        }
        case Failed(msg) => Failed(msg)
      }
    }

    /**
     * Отрицание следующего парсера
     * @param p парсер который нужно отрицать
     * @tparam U тип правового аргумента
     * @return
     */
    def not[U] (p: => Parser[U]): Parser[T] = Parser { in =>
      self.apply(in) match {
        case Success(resultL, readerL) => p(readerL) match {
          case Success(resultR, readerR) => Failed()
          case _ => Success(resultL, readerL)
        }
        case Failed(msg) => Failed(msg)
      }
    }

    /**
     * Понижает приоритет от '~'
     * @param p Parser
     * @tparam U тип правового аргумента
     * @return
     */
    def ^[U] (p: => Parser[U]): Parser[~[T,U]] = self.~(p)

    /**
     * Тоже самое что и ~ только не запоминает результат левого парсера
     * @param p правый парсер
     * @tparam U тип правового аргумента
     * @return
     */
    def ~>[U] (p: => Parser[U]): Parser[U] = Parser { in =>
      self.apply(in) match {
        case Success(resultL, readerL) => p(readerL) match {
          case Success(resultR, readerR) => Success(resultR, readerR)
          case Failed(msg) => Failed(msg)
        }
        case Failed(msg) => Failed(msg)
      }
    }

    /**
     * Тоже самое что и ~ только не запоминает результат правового парсера
     * @param p правый парсер
     * @tparam U тип правового аргумента
     * @return
     */
    def <~[U] (p: => Parser[U]): Parser[T] = Parser { in =>
      self.apply(in) match {
        case Success(resultL, readerL) => p(readerL) match {
          case Success(resultR, readerR) => Success(resultL, readerR)
          case Failed(msg) => Failed(msg)
        }
        case Failed(msg) => Failed(msg)
      }
    }

    /**
     * Логическое или. Если есть соответствие и левым парсером возвращаеться результат левого парсера, если нет то правого
     * @param p правый парсер
     * @tparam U тип правового аргумента
     * @return
     */
    def |[U >: T] (p: => Parser[U]): Parser[U] = Parser { in =>
      self.apply(in) match {
        case Failed(msg) => p(in)
        case s => s
      }
    }

    /**
     * Поиск сколько угодно повторений по текущему парсеру
     * @return
     */
    def * :Parser[List[T]] = Parser { in =>
      (self ~ self.*)(in) match {
        case Success(result, reader) =>  Success(result._1::result._2, reader)
        case _ => success(List())(in)
      }
    }

    /**
     * Поиск до rep повторений
     * @param rep количество повторений
     * @return
     */
    def * (rep: Int):Parser[List[T]] = Parser { in =>
      if (rep == 0) success(List())(in) else (self ~ self.*(rep-1))(in) match {
        case Success(result, reader) => Success(result._1::result._2, reader)
        case _ => Failed()
      }
    }

    /**
     * Поиск повторений в диапазоне от start до finish
     * @param start от
     * @param finish до
     * @return
     */
    def * (start: Int, finish: Int):Parser[List[T]] = Parser { in =>
      (self.*(start) ~ (self.?(finish) fn { _.flatten }))(in) match {
        case Success(result, reader) => Success(result._1:::result._2, reader)
        case _ => Failed()
      }
    }

    /**
     * Опционально значение rep раз
     * @param rep количество повторений
     * @return
     */
    def ? (rep: Int):Parser[List[Option[T]]] = Parser { in =>
      if (rep == 0) success(List())(in) else (self.? ~ self.?(rep-1))(in) match {
        case Success(result, reader) => Success(result._1::result._2, reader)
        case _ => Failed()//TODO такого не бывает
      }
    }

    /**
     * Опционально значение
     * @return
     */
    def ? :Parser[Option[T]] = Parser { in =>
      self.apply(in) match {
        case Success(result, reader) => Success(Some(result), reader)
        case _ => Success(None, in)
      }
    }

    /**
     * Трансформация результатов поиска
     * @param f функция преобразования
     * @tparam H возвращаемый тип
     * @return
     */
    def fn[H] (f: T => H): Parser[H] = Parser { in =>
      self.apply(in) match {
        case Success(result, reader) => Success(f(result), reader)
        case _ => Failed()
      }
    }

  }

}


