package parser

import org.scalatest.{Matchers, FreeSpec}

/**
 * Created by tim on 14.12.15.
 */
class CharsParsersTest extends FreeSpec with Matchers with CharsParsers {

  val digest = char(c => Character.isDigit(c))

  "And(~) parser" - {
    "when success:" - {
      "not be error" in {
        parse(char('+') ~ char('a'), "+a")
      }
      "return custom type" in {
        val plusA = char('+') ~ char('a') fn { case plus ~ a => (plus, a) }
        parse(plusA, "+a") shouldBe ('+', 'a')
      }
      "should be triple ~" in {
        val aPlusB = char('1') ~ char('+') ~ char('2') fn { case a ~ plus ~ b => (a, plus, b) }
        parse(aPlusB, "1+2") shouldBe ('1', '+', '2')
      }
    }
    "when failed:" - {
      "error parsing" in {
        val aPlusB = char('1') ~ char('+') ~ char('2')
        intercept[ParseException] {
          parse(aPlusB, "12")
        }
      }
    }
  }

  "Or(|) parser" - {
    val op = char('+') | char('-')
    "when success:" - {
      "not be error" in {
        parse(op, "+")
      }
      "return custom type" in {
        val opType = op fn { _.toString }
        parse(opType, "-") shouldBe "-"
      }
    }
    "when failed:" - {
      "error parsing" in {
        intercept[ParseException] {
          parse(op, "a")
        }
      }
    }
  }

  "Option(?) parser" - {
    val zeroOrOne = char('5').?
    val tripleOption = digest.?(3)
    "when success:" - {
      "math Some(5)" in {
        parse(zeroOrOne, "5") shouldBe Some('5')
      }
      "math custom type 5" in {
        val zeroOrOneType = zeroOrOne fn { _.map(_.toString.toInt) }
        parse(zeroOrOneType, "5") shouldBe Some(5)
      }
      "math triple option char" in {
        parse(tripleOption, "123") shouldBe "123".toList.map(Some(_))
      }
      "math triple option char with custom type" in {
        val tripleOptionType = tripleOption fn { _.flatten.mkString }
        parse(tripleOptionType, "123") shouldBe "123"
      }
    }

    "when failed:" - {
      "should be exception" in {
        intercept[ParseException] {
          parse(tripleOption, "12345")
        }
      }
    }
  }

  "Multiple(*) parser" - {
    val anyLength = digest.*
    val triple = digest.*(3)
    val range = digest.*(2,4)
    "when success:" - {
      "math max length" in {
        parse(anyLength, "1234567890") shouldBe "1234567890".toList
      }
      "math max length with custom type" in {
        val anyLengthType = anyLength fn { _.mkString }
        parse(anyLengthType, "1234567890") shouldBe "1234567890"
      }
      "math 123 into 1234567890 with custom type" in {
        val tripleType = triple fn { _.mkString }
        parse(tripleType, "123") shouldBe "123"
      }
      "math range 2 - 4" in {
        parse(range, "123") shouldBe "123".toList
      }
    }
    "when failed:" - {
      "error 123 into 1234567890" in {
        intercept[ParseException] {
          parse(triple, "1234567890")
        }
      }
      "error if not found triple" in {
        intercept[ParseException] {
          parse(triple, "12")
        }
      }
      "error if range not found" in {
        intercept[ParseException] {
          parse(range, "1")
        }
      }
    }
  }

  "Parse expression" - {
    case class Exp(num1: Int, op1: Char, num2: Int, op2: Char, num3: Int)
    val operator = char(c => "*/-+".contains(c))
    val num = digest.* fn { _.mkString.toInt }
    val exp = num ~ operator ~ num ~ operator ~ num fn {
      case a ~ b ~ c ~ d ~ e => Exp(a, b, c, d, e)
    }
    "when success:" - {
      "math 111*222-333" in {
        parse(exp, "111*222-333") shouldBe Exp(111, '*', 222, '-', 333)
      }
    }
    "when failed:" - {
      "error parsing" in {
        intercept[ParseException] {
          parse(exp, "111*222333")
        }
      }
    }
  }

  "Implicit string to Parser" - {
    "when success:" - {
      "math val string" in {
        parse("val", "val") shouldBe "val"
      }
      "math complex expression" in {
        val num = digest.* fn { _.mkString.toInt }
        val iflexem = "if" ~ char(' ').*.? ~ "(" ~ num ~ ")" fn {
          case lexem ~ spane ~ l ~ e ~ r => e
        }
        parse(iflexem, "if    (5)") shouldBe 5
      }
    }
    "when failed:" - {
      "error parsing" in {
        intercept[ParseException] {
          parse("val", "var")
        }
      }
    }
  }

}
