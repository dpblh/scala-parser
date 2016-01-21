package parser

/**
 * Created by tim on 14.12.15.
 */
class CharsReader(val source: String, val offset: Int) extends Reader with Iterator[CharsReader] {

  def this(s: String) = this(s, 0)

  def skip(offset: Int): CharsReader = new CharsReader(source, offset)

  def peek:Char = if (source.length > offset) source.charAt(offset) else Reader.EOF

  def hasNext: Boolean = peek != Reader.EOF

  def next():CharsReader = new CharsReader(source, offset + 1)

  def skipWhiteSpace: CharsReader = this

}
