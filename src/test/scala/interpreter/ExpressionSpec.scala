package interpreter

import org.scalatest.{FlatSpec, Matchers}

class ExpressionSpec extends FlatSpec with Matchers {
  "operations on numbers" should "work correctly" in {
    val a = Number(10.0)
    val b = Number(100)
    a + b shouldEqual Number(110.0)
    b - a shouldEqual Number(90.0)
    a * b shouldEqual Number(1000.0)
    a / b shouldEqual Number(0.1)
    a < b shouldEqual Symbol("t")
    a > b shouldEqual ListExpr(List())
    a == b shouldEqual ListExpr(List())
    a <= b shouldEqual Symbol("t")
    a >= b shouldEqual ListExpr(List())
  }

  "atom" should "give Symbol('t') for all atoms and empty list and ListExpr(List) for others" in {
    ListExpr(List()).atom shouldEqual Symbol("t")
    Number(10).atom shouldEqual Symbol("t")
    Symbol("t").atom shouldEqual Symbol("t")
    ListExpr(List(Symbol("+"), Number(10), Number(2))).atom shouldEqual ListExpr(List())
    Procedure(List(), ListExpr(List(Symbol("+"), Number(10), Number(2))), Environment(None, Map())).atom shouldEqual ListExpr(
      List()
    )
  }

  "eq" should "return Symbol('t') for similar atoms and empty lists ListExpr(List) for others" in {
    ListExpr(List()).eq(ListExpr(List())) shouldEqual Symbol("t")
    ListExpr(List()).eq(ListExpr(List(Symbol("+"), Number(10), Number(2)))) shouldEqual ListExpr(List())
    ListExpr(List(Symbol("+"), Number(10), Number(2)))
      .eq(ListExpr(List(Symbol("+"), Number(10), Number(2)))) shouldEqual ListExpr(List())
    Symbol("a").eq(Symbol("t")) shouldEqual ListExpr(List())
    Symbol("a").eq(Symbol("a")) shouldEqual Symbol("t")
    Number(10).eq(Number(10)) shouldEqual Symbol("t")
    Number(10).eq(Symbol("t")) shouldEqual ListExpr(List())
    Procedure(List(), ListExpr(List(Symbol("+"), Number(10), Number(2))), Environment(None, Map()))
      .eq(Number(10)) shouldEqual ListExpr(List())
  }

  "car for ListExpr" should "return first expression from list" in {
    ListExpr(List()).car shouldEqual ListExpr(List())
    ListExpr(List(Symbol("+"))).car shouldEqual Symbol("+")
    ListExpr(List(Symbol("-"), Number(10))).car shouldEqual Symbol("-")
    ListExpr(List(ListExpr(List(Symbol("-"), Number(10))), Symbol("-"), Number(10))).car shouldEqual ListExpr(
      List(Symbol("-"), Number(10))
    )
  }

  "cdr for ListExpr" should "return list expression from list containing all except first element" in {
    ListExpr(List()).cdr shouldEqual ListExpr(List())
    ListExpr(List(Symbol("+"))).cdr shouldEqual ListExpr(List())
    ListExpr(List(Symbol("-"), Number(10))).cdr shouldEqual ListExpr(List(Number(10)))
    ListExpr(List(ListExpr(List(Symbol("-"), Number(10))), Symbol("-"), Number(10))).cdr shouldEqual ListExpr(
      List(Symbol("-"), Number(10))
    )
  }

  "cons" should "adding left element to right list" in {
    Symbol("abc").cons(ListExpr(List(Symbol("+")))) shouldEqual (ListExpr(List(Symbol("abc"), Symbol("+"))))
    Number(10).cons(ListExpr(List())) shouldEqual ListExpr(List(Number(10)))
    ListExpr(List(Symbol("-"), Number(10))).cons(ListExpr(List(Symbol("abc")))) shouldEqual ListExpr(
      List(ListExpr(List(Symbol("-"), Number(10))), Symbol("abc"))
    )
  }
}
