package interpreter

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Try, Success, Failure}

class EvalSpec extends FlatSpec with Matchers {
  val env: Environment = Environment(None, Map())

  "operations on numbers" should "work properly" in {
    Eval(Parser("(+ 1 2 3)"), env) shouldEqual Number(6)
    Eval(Parser("(+ 1 (* 2 3 4))"), env) shouldEqual Number(25)
    Eval(Parser("(+ 1 2 (- 2 3))"), env) shouldEqual Number(2)
    Eval(Parser("(/ 1 2 2)"), env) shouldEqual Number(0.25)
    Eval(Parser("(/ 2)"), env) shouldEqual Number(2)
    Eval(Parser("(- 10)"), env) shouldEqual Number(-10)
    Eval(Parser("(/ 10 5 2)"), env) shouldEqual Number(1)
    Eval(Parser("(- 10 2 3 6)"), env) shouldEqual Number(-1)
    Eval(Parser("(- 10 20 (* 10 100) (/ 1 (- 2)))"), env) shouldEqual Number(-1009.5)
    Eval(Parser("(< 10 20)"), env) shouldEqual Symbol("t")
    Eval(Parser("(> 10 20)"), env) shouldEqual ListExpr(List())
    Eval(Parser("(<= 10 20)"), env) shouldEqual Symbol("t")
    Eval(Parser("(== 10 20)"), env) shouldEqual ListExpr(List())
    Eval(Parser("(== 10 10)"), env) shouldEqual Symbol("t")
    Eval(Parser("(== 10 (/ 100 10))"), env) shouldEqual Symbol("t")
    Try(Eval(Parser("(== abc (/ 100 10))"), env)) match {
      case Success(_)     => assert(false)
      case Failure(value) => assert(true)
    }
    Try(Eval(Parser("(== 10 (/ 100 a))"), env)) match {
      case Success(_) => assert(false)
      case Failure(_) => assert(true)
    }
  }

  "atoms" should "return themselves" in {
    Eval(Parser("10"), env) shouldEqual Number(10)
    Eval(Parser("10.12"), env) shouldEqual Number(10.12)
    Eval(Parser("()"), env) shouldEqual ListExpr(List())
  }

  "defining new variable" should "create binding in evnironment" in {
    Eval(Parser("(define speed (* 3 1000000))"), env) shouldEqual Number(3000000)
    env.find("speed") shouldEqual Number(3000000)
    Eval(Parser("speed"), env) shouldEqual Number(3000000)
    Try(Eval(Parser("(speed)"), env)) match {
      case Success(_) => assert(false)
      case Failure(_) => assert(true)
    }
  }

  "setting existing variable" should "set new value to variable" in {
    Eval(Parser("(define speed (* 3 1000000))"), env) shouldEqual Number(3000000)
    Eval(Parser("(set! speed 200)"), env) shouldEqual Number(3000000)
    env.find("speed") shouldEqual Number(200)
    Eval(Parser("speed"), env) shouldEqual Number(200)
    Try(Eval(Parser("(set! x 200)"), env)) match {
      case Success(_) => assert(false)
      case Failure(_) => assert(true)
    }
  }

  "basic operations" should "work properly" in {
    Eval(Parser("(quote a)"), env) shouldEqual Symbol("a")
    Eval(Parser("(quote (a b c))"), env) shouldEqual ListExpr(List(Symbol("a"), Symbol("b"), Symbol("c")))
    Eval(Parser("(atom (q a))"), env) shouldEqual Symbol("t")
    Eval(Parser("(atom (q (a b c)))"), env) shouldEqual ListExpr(List())
    Eval(Parser("(atom (q ()))"), env) shouldEqual Symbol("t")
    Eval(Parser("(atom (atom (q a)))"), env) shouldEqual Symbol("t")
    Eval(Parser("(atom (q (atom (q a))))"), env) shouldEqual ListExpr(List())
    Eval(Parser("(eq (q a) (q a))"), env) shouldEqual Symbol("t")
    Eval(Parser("(eq (q a) (q b))"), env) shouldEqual ListExpr(List())
    Eval(Parser("(car (q (a b c)))"), env) shouldEqual Symbol("a")
    Eval(Parser("(cdr (q (a b c)))"), env) shouldEqual ListExpr(List(Symbol("b"), Symbol("c")))
    Eval(Parser("(cons (q a) (q (b c)))"), env) shouldEqual ListExpr(List(Symbol("a"), Symbol("b"), Symbol("c")))
    Eval(Parser("(cons (q a) (cons (q b) (cons (q c) ())))"), env) shouldEqual ListExpr(
      List(Symbol("a"), Symbol("b"), Symbol("c"))
    )
    Eval(Parser("(cons (q a) (cons (q b) (cons (q c) (q ()))))"), env) shouldEqual ListExpr(
      List(Symbol("a"), Symbol("b"), Symbol("c"))
    )
    Eval(Parser("(car (cons (q a) (q (b c))))"), env) shouldEqual Symbol("a")
    Eval(Parser("(cdr (cons (q a) (q (b c))))"), env) shouldEqual ListExpr(List(Symbol("b"), Symbol("c")))
    Eval(Parser("(cond ((eq (q a) (q b)) (q first)) ((atom (q a)) (q second)))"), env) shouldEqual Symbol("second")
  }

  "begin" should "work properly" in {
    Eval(Parser("(begin (+ 1 2) (* 2 3) (cdr (cons (q a) (q (b c)))) (cond ((eq (q a) (q b)) (q first)) ((atom (q a)) (q second))))"), env) shouldEqual Symbol("second")
  }

  "lambda" should "create function" in {
    Eval(Parser("((lambda (x) (cons x (q (b)))) (q a))"), env) shouldEqual ListExpr(List(Symbol("a"), Symbol("b")))
    Eval(Parser("((lambda (x y) (cons x (cdr y))) (q z) (q (a b c)))"), env) shouldEqual ListExpr(List(Symbol("z"), Symbol("b"), Symbol("c")))
    Eval(Parser("((lambda (f) (f (q (b c)))) (q (lambda (x) (cons (q a) x))))"), env) shouldEqual ListExpr(List(Symbol("a"), Symbol("b"), Symbol("c")))
    Eval(Parser("((lambda (f) (f (q (b c)))) (lambda (x) (cons (q a) x)))"), env) shouldEqual ListExpr(List(Symbol("a"), Symbol("b"), Symbol("c")))
  }
}
