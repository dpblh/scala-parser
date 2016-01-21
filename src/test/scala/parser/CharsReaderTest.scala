package parser

import org.scalatest.FlatSpec

/**
 * Created by tim on 11.12.15.
 */
class CharsReaderTest extends FlatSpec {

  it should "return EOF" in {
    val reader = new CharsReader("")
    assert(reader.peek == Reader.EOF)
  }

  it should "return char a" in {
    val reader = new CharsReader("abs")
    assert(reader.peek == 'a')
  }

  it should "return b" in {
    val reader = new CharsReader("abc").next()
    assert(reader.peek == 'b')
  }
  it should "sequence a-b-c-EOF" in {
    val reader = new CharsReader("abc")
    assert(reader.peek == 'a')
    val readB = reader.next()
    assert(readB.peek == 'b')
    val readC = readB.next()
    assert(readC.peek == 'c')
    val readEOF = readC.next()
    assert(readEOF.peek == Reader.EOF)
  }

  it should "return true if reader end" in {
    val reader = new CharsReader("")
    assert(!reader.hasNext)
    val reader2 = new CharsReader("a").next()
    assert(!reader2.hasNext)
  }

}
