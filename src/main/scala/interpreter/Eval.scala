package interpreter

object Eval {

  val numberOperations: Map[String, Set[String]] =
    Map("comparison" -> Set("<", ">", "<=", ">=", "=="), "arithmetic" -> Set("+", "-", "*", "/"))

  @scala.annotation.tailrec
  def evcon(cond: Expression, env: Environment): Expression = {
    eval(cond.car.car, env) match {
      case Symbol("t") => eval(cond.car.cdr.car, env)
      case _           => evcon(cond.cdr, env)
    }
  }

  def arithmeticOperation(op: String, args: ListExpr, env: Environment): Expression = {
    def applyOperation(init: Number)(op: (Number, Number) => Number): Number = {
      @scala.annotation.tailrec
      def reduce(l: List[Expression], acc: Number): Number = l match {
        case List() => acc
        case x :: rest =>
          eval(x, env) match {
            case a: Number => reduce(rest, op(acc, a))
            case _         => throw new IllegalArgumentException("arithmetic operation is applied not to number")
          }
      }
      reduce(args.v, init)
    }

    op match {
      case "+" => applyOperation(Number(0))(_ + _)
      case "-" =>
        if (args.cdr == ListExpr(List()))
          args.car match {
            case n: Number => Number(0) - n
            case _         => throw new IllegalArgumentException("arithmetic operation is applied not to number")
          }
        else
          args.car match {
            case n: Number => applyOperation(n)((a, b) => a - b)
            case _         => throw new IllegalArgumentException("arithmetic operation is applied not to number")
          }
      case "*" => applyOperation(Number(1))(_ * _)
      case "/" => applyOperation(Number(1))((a, b) => a / b)
      case _   => throw new IllegalArgumentException("Unknown arithmetic operator")
    }
  }

  def comparisonOperation(op: String, operand1: Expression, operand2: Expression, env: Environment): Expression =
    op match {
      case "<" =>
        eval(operand1, env) match {
          case a: Number =>
            eval(operand1, env) match {
              case b: Number => a < b
              case _         => throw new IllegalArgumentException("second argument must be Number")
            }
          case _ => throw new IllegalArgumentException("first argument must be Number")
        }
      case ">" =>
        eval(operand2, env) match {
          case a: Number =>
            eval(operand1, env) match {
              case b: Number => a > b
              case _         => throw new IllegalArgumentException("second argument must be Number")
            }
          case _ => throw new IllegalArgumentException("first argument must be Number")
        }
      case "==" =>
        eval(operand1, env) match {
          case a: Number =>
            eval(operand1, env) match {
              case b: Number => a == b
              case _         => throw new IllegalArgumentException("second argument must be Number")
            }
          case _ => throw new IllegalArgumentException("first argument must be Number")
        }
      case "<=" =>
        eval(operand1, env) match {
          case a: Number =>
            eval(operand1, env) match {
              case b: Number => a <= b
              case _         => throw new IllegalArgumentException("second argument must be Number")
            }
          case _ => throw new IllegalArgumentException("first argument must be Number")
        }
      case ">=" =>
        eval(operand1, env) match {
          case a: Number =>
            eval(operand1, env) match {
              case b: Number => a >= b
              case _         => throw new IllegalArgumentException("second argument must be Number")
            }
          case _ => throw new IllegalArgumentException("first argument must be Number")
        }
    }

  def eval(expr: Expression, env: Environment): Expression = expr match {
    case _: Number        => expr
    case Symbol(x)        => env.find(x)
    case ListExpr(List()) => expr
    case e: ListExpr =>
      e.car match {
        case Symbol("quote") | Symbol("q") => e.cdr.car
        case Symbol("atom")                => eval(e.cdr.car, env).atom
        case Symbol("eq")                  => eval(e.cdr.car, env).eq(eval(e.cdr.cdr.car, env))
        case Symbol("car")                 => eval(e.cdr.car, env).car
        case Symbol("cdr")                 => eval(e.cdr.car, env).cdr
        case Symbol("cons")                => eval(e.cdr.car, env).cons(eval(e.cdr.cdr.car, env))
        case Symbol("cond")                => evcon(e.cdr, env)
        case Symbol("begin") =>
          val exprs = e.cdr
          exprs.v.map(eval(_, env)).last
        case Symbol("define") =>
          e.cdr.car match {
            case Symbol(x) => env.define(x, eval(e.cdr.cdr.car, env))
            case _         => throw new IllegalArgumentException("wrong name")
          }
        case Symbol("set!") =>
          e.cdr.car match {
            case Symbol(x) => env.set(x, eval(e.cdr.cdr.car, env))
            case _         => throw new IllegalArgumentException("wrong name")
          }
        case Symbol("lambda") =>
          e.cdr.car match {
            case ListExpr(args) =>
              val names = args.map({
                case Symbol(x) => x
                case _         => throw new IllegalArgumentException("wrong name for procedure args")
              })
              Procedure(names, e.cdr.cdr.car, env)
            case _ => throw new IllegalArgumentException("wrong list of params")
          }
        case Symbol(x) if numberOperations("comparison").contains(x) =>
          comparisonOperation(x, e.cdr.car, e.cdr.cdr.car, env)
        case Symbol(x) if numberOperations("arithmetic").contains(x) =>
          arithmeticOperation(x, e.cdr, env)
        case Symbol(_) =>
          eval(e.car, env) match {
            case Procedure(args, body, env) =>
              def pair(e1: List[String], e2: ListExpr): Map[String, Expression] = {
                val l2 = e2.v
                e1.zip(l2).map(x => (x._1, eval(x._2, env))).toMap
              }
              val innerEnv: Map[String, Expression] = pair(args, e.cdr)
              eval(body, Environment(Some(env), innerEnv))
            case _ => throw new IllegalArgumentException("unknown procedure")
          }
        case _ => throw new IllegalArgumentException("wrong lisp expression")
      }
    case _ => throw new IllegalArgumentException("wrong lisp expression")
  }

  def apply(expr: Expression, env: Environment): Expression = {
    eval(expr, env)
  }

}
