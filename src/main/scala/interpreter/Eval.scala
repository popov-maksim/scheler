package interpreter

object Eval {

  /**
   * Evaluating value of list expression
   *
   * @param op function or operator
   * @param rest arguments to apply function or operator
   * @param env current environment
   * @return resulted expression
   */
  def evaluateListExpr(op: String, rest: Seq[Expression], env: Environment): Expression = op match {
    case "q" | "quote" => rest match {
      case arg :: Nil => arg
      case _ => throw new IllegalArgumentException(s"Expected exactly one argument to quote")
    }
    case "atom" => rest match {
      case arg :: Nil => eval(arg, env) match {
        case _: Atom => Symbol("t")
        case ListExpr(List()) => Symbol("t")
        case _ => ListExpr(List())
      }
      case _ => throw new IllegalArgumentException(s"Expected exactly one argument to atom")
    }
    case "eq" => rest match {
      case first :: second :: Nil => (eval(first, env), eval(second, env)) match {
        case (a1: Atom, a2: Atom) if a1 == a2 => Symbol("t")
        case (ListExpr(List()), ListExpr(List())) => Symbol("t")
        case _ => ListExpr(List())
      }
      case _ => throw new IllegalArgumentException(s"Expected exactly two argument to eq")
    }
    case "car" => rest match {
      case expr :: Nil => eval(expr, env) match {
        case ListExpr(l) => l.head
        case _ => throw new IllegalArgumentException(s"Expected list argument to car")
      }
      case _ => throw new IllegalArgumentException(s"Expected exactly one argument to car")
    }
    case "cdr" => rest match {
      case expr :: Nil => eval(expr, env) match {
        case ListExpr(l) => ListExpr(l.tail)
        case _ => throw new IllegalArgumentException(s"Expected list argument to cdr")
      }
      case _ => throw new IllegalArgumentException(s"Expected exactly one argument to cdr")
    }
    case "cons" => rest match {
      case "" => ???
    }
  }

  /**
   * Evaluating value of S-expression
   *
   * @param expr expression to evaluate
   * @param env current environment
   * @return resulted expression
   */
  def eval(expr: Expression, env: Environment): Expression = expr match {
    case Number(_) => expr
    case Symbol(x) => env.find(x)
    case ListExpr(l) => l match {
      case first :: rest => first match {
        case Symbol(op) => evaluateListExpr(op, rest, env)
      }
    }
  }

  def apply(expr: Expression, env: Environment): Expression = {
    eval(expr, env)
  }
}