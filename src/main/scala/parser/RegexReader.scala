package parser

/**
 * Created by tim on 14.12.15.
 */
class RegexReader(val source: String, val offset: Int) extends Reader with Iterator[RegexReader] {

  val whiteSpace = "\\s+".r
  
  def this(s: String) = this(s, 0)

  def skip(offset: Int): RegexReader = new RegexReader(source, offset)

  def peek:Char = if (source.length > offset) source.charAt(offset) else Reader.EOF

  def hasNext: Boolean = peek != Reader.EOF

  def next():RegexReader = new RegexReader(source, offset + 1)

  def skipWhiteSpace: RegexReader = {
    whiteSpace.findPrefixMatchOf(source.subSequence(offset, source.length)) match {
      case Some(x) => skip(offset + x.end)
      case _ => this
    }
  }

}