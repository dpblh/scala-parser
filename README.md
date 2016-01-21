# Scala Parser

@see https://github.com/dpblh/script-graphics

Реализация универсального парсера на scala. В основе лежат два логических действия "и", "или".

Имеются две реализации парсера:
- CharParsers
- RegexParsers

### RegexParsers
Имеет в наличии два имплисита String -> Parser, Regexp -> Parser

Проще всего показать работу парсера на примере
#### Пример
```scala
object SimpleParsers extends RegexpParsers {

  /**
   * Парсер поиска переменных
   */
  def variable = """([a-zA-Z]|\d|_)+""".r  fn NameLiteral

  /**
   * Парсер поиска циклов
   */
  def loop = ("for" ~> ("(" ~> variable ~ variable <~ ")") <~ "in") ~ variable ~ statement fn {
    case property ~ value ~ from ~ body => Loop(property, value, from, body)
  }

  /**
   * Парсер поиска утверждений
   */
  def statement: Parser[Statement] = block | loop

  /**
   * Парсер поиска блоков утверждений
   */
  def block = "{" ~> statement.* <~ "}" fn {
    case body => Block(body)
  }

  def main(args: Array[String]) {
    parse(loop, "for (property value) in object {}")
    parse(loop,
      """for (property value) in object {
        | for (property value) in object {}
        |}""".stripMargin)
  }

}

case class NameLiteral(a: String)
trait Statement
case class Block(statements: List[Statement]) extends Statement
case class Loop(property: NameLiteral, value: NameLiteral, from: NameLiteral, body: Statement) extends Statement
```
Проанализируем код
- variable имплисит преобразование из регулярки в парсер и преобразование в NameLiteral
- statement & block рекурсивно завязаны друг на друга. statement может быть либо одиночным циклом либо блоком циклов. Работает благодаря таму что методы парсера (| ~ ...) принимает не парсер а функцию возвращающую парсер.
- loop самое интересное. Содержит сразу несколько комбинаций парсеров. "for" ~> просто строка которая не запоминаеться в результат. "(" ~> variable ~ variable <~ ")" переменные в которые будут занесены результаты название свойства и его значения. <~ "in" незапоменающая строка. variable - по какой переменной будет осуществляться итерация по свойствам. statement кусок кода который болжен быть выполнен на каждой итерации

#### Доступные методы парсера
- "~" Логическое объединение парсеров
- "|" Логическое или
- "~>" Логическое объединение парсеров без запоминания результата левого парсера
- "<~" Логическое объединение парсеров без запоминания результата правого парсера
- "*" Поиск сколько угодно повторений по текущему парсеру
- "?" Опционально значение
- "fn" Трансформация результатов поиска
- Больше парсеров в документации к коду
