package interpreter

object Parser {

  def apply(str: String): Expression = parse(tokenize(str))

  /**
    *  Parse tokens into structure for further evaluation
    *
    * @param tokens tokens gotten from tokenized input
    * @return
    */
  def parse(tokens: Seq[String]): Expression = {

    // Parse a list from the given sequence, returning this and the number of tokens consumed.
    def parseTokens(tokens: Seq[String], count: Int, openCounter: Int, exprs: List[Expression]): (Int, Int, List[Expression]) = tokens match {
      case ")" +: Seq() => if (openCounter != 0) throw new IllegalArgumentException("Unbalanced parentheses") else (count + 1, openCounter - 1, exprs.reverse)
      case ")" +: _ => if (openCounter < 1) throw new IllegalArgumentException("Unbalanced parentheses") else (count + 1, openCounter - 1, exprs.reverse)
      case "(" +: rest =>
        val (numTokens, opened, parsedTokens) = parseTokens(rest, 1, openCounter + 1, List())
        parseTokens(tokens drop numTokens, count + numTokens, opened, ListExpr(parsedTokens) :: exprs)
      case token +: rest => parseTokens(rest, count + 1, openCounter, atom(token) :: exprs)
    }

    parseTokens(tokens :+ ")", 0, 0, List())._3.head

  }

  /**
    * Splitting input command into tokens for further parsing
    *
    * @param str string to extract tokens from
    * @return
    */
  def tokenize(str: String): Array[String] =
    str.replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").split("\\s").filter(!_.isEmpty)

  /**
    * Converting string to atom
    *
    * @param token string for converting
    * @return
    */
  def atom(token: String): Atom =
    Number(token).getOrElse(Symbol(token))
}
