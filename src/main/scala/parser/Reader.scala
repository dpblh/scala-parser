package parser

/**
 * Created by tim on 23.12.15.
 */
trait Reader {

  this: Iterator[Reader] =>

  val source: String
  val offset: Int

  def skip(offset: Int): Reader
  def peek:Char
  def hasNext: Boolean
  def next(): Reader
  def skipWhiteSpace: Reader

}

object Reader {
  val EOF: Char = '\u001a'
}
