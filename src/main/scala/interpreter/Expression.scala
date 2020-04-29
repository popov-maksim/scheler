package interpreter

import scala.util.Try

sealed abstract class Expression {
  val v: Any
  def atom: Expression = ListExpr(List())
  def eq(other: Expression): Expression
  def car: Expression = throw new IllegalArgumentException("car must be applied to list expression")
  def cdr: Expression = throw new IllegalArgumentException("cdr must be applied to list expression")
  def cons(e: Expression): ListExpr = e match {
    case ListExpr(l) => ListExpr(this :: l)
    case _           => throw new IllegalArgumentException("second argument must be list")
  }
}

sealed abstract class Atom extends Expression {
  val v: Any
  override def atom: Expression = Symbol("t")
  override def eq(other: Expression): Expression = other match {
    case a: Atom if v == a.v => Symbol("t")
    case _                   => ListExpr(List())
  }
}

final case class Number(v: BigDecimal) extends Atom {
  override def toString: String      = v.toString
  def +(number: Number): Number      = Number(v + number.v)
  def -(number: Number): Number      = Number(v - number.v)
  def *(number: Number): Number      = Number(v * number.v)
  def /(number: Number): Number      = Number(v / number.v)
  def <(number: Number): Expression  = if (v < number.v) Symbol("t") else ListExpr(List())
  def >(number: Number): Expression  = if (v > number.v) Symbol("t") else ListExpr(List())
  def ==(number: Number): Expression = if (v == number.v) Symbol("t") else ListExpr(List())
  def <=(number: Number): Expression = if (v <= number.v) Symbol("t") else ListExpr(List())
  def >=(number: Number): Expression = if (v >= number.v) Symbol("t") else ListExpr(List())
}
object Number {
  def apply(v: String): Try[Number] = {
    Try(BigDecimal(v)).map(Number(_))
  }
}

final case class Symbol(v: String) extends Atom {
  override def toString: String = v
}

final case class ListExpr(v: List[Expression]) extends Expression with Iterable[Expression] {
  override def toString: String               = v.mkString("(", " ", ")")
  override def iterator: Iterator[Expression] = v.iterator
  override def atom: Expression = v match {
    case List() => Symbol("t")
    case _      => ListExpr(List())
  }
  override def car: Expression = v match {
    case List() => ListExpr(List())
    case _      => v.head
  }
  override def cdr: ListExpr = v match {
    case List() => ListExpr(List())
    case _      => ListExpr(v.tail)
  }
  def eq(other: Expression): Expression = other match {
    case ListExpr(List()) if v == List() => Symbol("t")
    case _                               => ListExpr(List())
  }

}

case class Procedure(args: List[String], v: Expression, env: Environment) extends Expression {
  override def eq(other: Expression): Expression = ListExpr(List())
  override def toString: String                  = "procedure: " + s"Args: $args" + " " + s"Body: $v"
}
