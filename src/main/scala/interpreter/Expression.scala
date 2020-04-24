package interpreter

import scala.util.Try

sealed trait Expression

trait Atom extends Expression

final case class Number[A: Numeric](v: A) extends Atom {
  override def toString: String = v.toString
}
object Number {
  def apply[T: Numeric](v: String, f: String => T): Try[Number[T]] = {
    Try(f(v)).map(Number(_))
  }
}

final case class Symbol(v: String) extends Atom {
  override def toString: String = v
}

final case class ListExpr(v: List[Expression]) extends Expression with Iterable[Expression] {
  override def toString: String = v.mkString("(", " ", ")")
  override def iterator: Iterator[Expression] = v.iterator
}
