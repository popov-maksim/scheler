package interpreter

import scala.collection.mutable

case class Environment(outer: Option[Environment], presets: Map[String, Expression])
    extends mutable.HashMap[String, Expression] {
  
  def this(outer: Option[Environment]) = this(outer, Map())
  def this(outer: Option[Environment], vars: List[String], values: Seq[Expression]) =
    this(outer, Map() ++ vars.zip(values))

  this ++= presets

  def find(name: String): Expression = {
    if (contains(name)) apply(name)
    else
      outer match {
        case None    => throw new IllegalArgumentException(s"undefined symbol '$name'")
        case Some(e) => e.find(name)
      }
  }

  def define(name: String, value: Expression): Expression = {
    this += (name -> value)
    value
  }

  def set(name: String, value: Expression): Expression = {
    val previous: Expression = find(name)
    this += (name -> value)
    previous
  }
}
