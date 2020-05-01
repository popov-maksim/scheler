package interpreter

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}

object Repl extends App {

  /**
    * Running REPL: reading commands from standard input and executing them
    */
  def run(prompt: String = "scheler> "): Unit = {
    while (true) {
      print(prompt)
      val input = readLine()
      Try(Parser(input)) match {
        case Success(expr) => println(Try(Eval(expr, globalEnvironment)) match {
          case Success(v) => v
          case Failure(e) => e.getMessage
        })
        case Failure(e) => println(e.getMessage)
      }
    }
  }

  /**
    * Load expressions from file and evaluate them
    *
    * @param filename file to load from
    * @param env current environment
    */
  def load(filename: String, env: Environment): Unit = {
    val source = Source.fromFile(filename)
    val code   = source.getLines().mkString(" ")

    println("... Loading and executing given file ...\n")

    @scala.annotation.tailrec
    def read(index: Integer, counter: Integer, s: String, result: List[String]): List[String] = {
      if (index == code.length)
        result.reverse
      else {
        val newS: String = if (code.charAt(index) != '\n') s + code.charAt(index) else s
        val newCounter: Integer =
          if (code.charAt(index) == '(')
            counter + 1
          else if (code.charAt(index) == ')')
            counter - 1
          else
            counter
        if (newCounter == 0 && index > 0) {
          val linecode = newS.trim.replaceAll(" +", " ")
          read(index + 1, newCounter, "", if (linecode != " " && linecode != "") linecode :: result else result)
        } else
          read(index + 1, newCounter, newS, result)
      }
    }
    for (linecode <- read(0, 0, "", List()))
      Eval(Parser(linecode), globalEnvironment)
  }

  val globalEnvironment: Environment = Environment(None, Map())

  if (args.length == 1)
    load(args(0), globalEnvironment)

  run()

}
