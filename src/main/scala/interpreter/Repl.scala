package interpreter

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.{ Try, Success, Failure }

object Repl extends App {

  /**
   * Getting commands from the user
   *
   * @param prompt string to be shown at every line for input
   * @return lazy list of commands given by the user
   */
  def inputs(prompt: String = "scheler> "): LazyList[String] = {
    print(prompt)
    readLine() match {
      case null => LazyList()
      case str => str #:: inputs()
    }
  }

  /**
   * Running REPL: reading commands from standard input and executing them
   */
  def run(): Unit = {
    for (input <- inputs())
      for (expr <- Parser(input))
        println(Try(Eval(expr, globalEnv)) match {
          case Success(v) => v
          case Failure(e) => e.getMessage
        })
  }

  /**
   * Load expressions from file and evaluate them
   *
   * @param filename file to load from
   * @param env current environment
   */
  def load(filename: String, env: Environment): Unit = {
    val source = Source.fromFile(filename)
    for (input <- source.getLines())
      Eval(Parser(input), env)
    source.close()
  }

  run()

}
