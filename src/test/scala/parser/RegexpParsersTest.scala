package parser

import org.scalatest.{Matchers, FreeSpec}

/**
 * Created by tim on 19.12.15.
 */
class RegexpParsersTest extends FreeSpec with Matchers with RegexpParsers {

  "Test string lexeme" - {
    val name = "val"
    val exp = "a" ~ "=" ~ "1" fn { case a ~ eq ~ one => s"$a$eq$one" }
    val variable = "val" | "var"
    val complexExp = variable ~ exp fn { case v ~ e => s"$v $e" }
    "match val string" in {
      parse(name, "val ") shouldBe "val"
    }
    "match val with prefix" in {
      parse(name, "    val ") shouldBe "val"
    }
    "match exp" in {
      parse(exp, "a=1") shouldBe "a=1"
    }
    "match exp with space" in {
      parse(exp, "  a  =  1 ") shouldBe "a=1"
    }
    "match val or var" in {
      parse(variable, "  var ") shouldBe "var"
    }
    "match complex exp (val a = 1)" in {
      parse(complexExp, "  val   a=1 ") shouldBe "val a=1"
    }
  }

  "Test regex lexeme" - {
    val identifier = """([a-z]|_)+""".r
    val value = "('\\w*'|\\d*)".r | identifier
    val exp = "val" ~ identifier ~ "=" ~ value fn { case v ~ id ~ eq ~ value2 => value2 }
    val or = exp | value
    "match identifier" in {
      parse(identifier, "regexp_s") shouldBe "regexp_s"
    }
    "match exp" in {
      parse(exp, "val  string = 'success'") shouldBe "'success'"
    }
    "match regex or(|)" in {
      parse(or, "123") shouldBe "123"
    }
  }

}
