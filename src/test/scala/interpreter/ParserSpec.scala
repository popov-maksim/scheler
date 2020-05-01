package interpreter

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Try, Success, Failure}

class ParserSpec extends FlatSpec with Matchers {
  "parser" should "return parsed expression" in {
    Parser("abc") shouldEqual Symbol("abc")
    Parser("10") shouldEqual Number(10)
    Parser("112.12") shouldEqual Number(112.12)
    Parser("(+ 1 2 3 4)") shouldEqual ListExpr(List(Symbol("+"), Number(1), Number(2), Number(3), Number(4)))
    Parser("(- 1 (+ 2 3))") shouldEqual ListExpr(
      List(Symbol("-"), Number(1), ListExpr(List(Symbol("+"), Number(2), Number(3))))
    )
    Parser("()") shouldEqual ListExpr(List())
    Parser("(- 1 ((quote 1 2 3) (quote (quote 1 2) 1 2)))") shouldEqual
      ListExpr(
        List(
          Symbol("-"),
          Number(1),
          ListExpr(
            List(
              ListExpr(List(Symbol("quote"), Number(1), Number(2), Number(3))),
              ListExpr(
                List(Symbol("quote"), ListExpr(List(Symbol("quote"), Number(1), Number(2))), Number(1), Number(2))
              )
            )
          )
        )
      )
  }

  "parser" should "throw exception on unbalanced with respect to parentheses expression" in {
    val errMsg = "Unbalanced parentheses"
    Try(Parser("(")) match {
      case Success(_)     => assert(false)
      case Failure(value) => value.getMessage shouldEqual errMsg
    }
    Try(Parser("(abc")) match {
      case Success(_)     => assert(false)
      case Failure(value) => value.getMessage shouldEqual errMsg
    }
    Try(Parser("(abc))")) match {
      case Success(_)     => assert(false)
      case Failure(value) => value.getMessage shouldEqual errMsg
    }
    Try(Parser("abc)")) match {
      case Success(_)     => assert(false)
      case Failure(value) => value.getMessage shouldEqual errMsg
    }
    Try(Parser("(+ 1 2 3()")) match {
      case Success(_)     => assert(false)
      case Failure(value) => value.getMessage shouldEqual errMsg
    }
    Try(Parser("(+ 1 2 3()))))))")) match {
      case Success(_)     => assert(false)
      case Failure(value) => value.getMessage shouldEqual errMsg
    }
    Try(Parser(")")) match {
      case Success(_)     => assert(false)
      case Failure(value) => value.getMessage shouldEqual errMsg
    }
    Try(Parser("(+ 1 2))")) match {
      case Success(_)     => assert(false)
      case Failure(value) => value.getMessage shouldEqual errMsg
    }
  }
}
