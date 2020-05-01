package interpreter

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Try, Success, Failure}

class EnvironmentSpec extends FlatSpec with Matchers {
  val env: Environment = Environment(None, Map());

  "find(name) on empty environment" should "give exception" in {
    Try(env.find("x")) match {
      case Success(v) => assert(false)
      case Failure(v) => v.getMessage shouldEqual s"undefined symbol 'x'"
    }
  }

  "define(name, expression)" should "define new binding" in {
    env.define("x", Number(10.0)) shouldEqual Number(10.0)
    Try(env.find("x")) match {
      case Success(v) => v shouldEqual Number(10.0)
      case Failure(v) => assert(false)
    }
  }

  "set(name, expression)" should "change value of existing variable" in {
    env.set("x", Number(100)) shouldEqual Number(10.0)
    Try(env.find("x")) match {
      case Success(v) => v shouldEqual Number(100)
      case Failure(v) => assert(false)
    }
  }
}
