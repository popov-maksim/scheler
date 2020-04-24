package interpreter

class Environment(env: Map[String, Expression], outer: Option[Environment]) {

  /**
   * Find the expression associated with given name
   *
   * @param name name to find
   * @return found expression or error
   */
  def find(name: String): Expression = {
    if (env.contains(name)) env(name)
    else
      outer match {
        case None    => throw new IllegalArgumentException(s"Undefined symbol '$name'")
        case Some(e) => e.find(name)
      }
  }

  /**
   * Set expression for given name, change if was before
   *
   * @param name name to set expression for
   * @param value expression for given name
   * @return updated environment
   */
  def set(name: String, value: Expression): Environment = {
    Environment(env + (name -> value), outer)
  }
}

object Environment {
  def apply(env: Map[String, Expression], outer: Option[Environment] = None): Environment = {
    new Environment(env, outer)
  }
}
