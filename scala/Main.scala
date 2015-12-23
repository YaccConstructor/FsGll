import com.codecommit.gll.{Parsers, RegexParsers}

/**
 * Created by user on 23.10.2015.
 */


object GrammarNNN extends Parsers {
  def atom : Parser[String] = literal("0")
  lazy val expr: Parser[String] = expr ~ expr ~ expr ^^ { _ + _ + _ } | expr ~ expr ^^ { _ + _ } | atom
}


object GrammarExtCalc extends RegexParsers {
  abstract class AST
  case class EAdditive(l: AST, op: String, r: AST) extends AST
  case class EMultiplicative(l: AST, op: String, r: AST) extends AST
  case class EUnary(op: String, r: AST) extends AST
  case class EVar(a: String) extends AST
  case class EVal(x: Double) extends AST
  case class Stmt(v: EVar, e: AST) extends AST
  case class Pgm(s: List[Stmt], e: AST) extends AST

  def bws[A](p: Parser[A]) = (literal(" ")*) ~> p <~ (literal(" ")*)

  lazy val digits = (("[0-9]".r)+) ^^ { _.mkString("") }
  lazy val notDigit : Parser[String] = "[a-zA-Z_]".r
  lazy val sym : Parser[String] = "[0-9a-zA-Z_]".r

  lazy val value = (digits ~ ((literal(".") ~ digits ^^ { _ + _ })?)) ^^ { (a, b) => EVal((a + b.getOrElse("")).toDouble) }
  lazy val variable : Parser[EVar] = bws(notDigit ~ (sym*)) ^^ ( (a, b) => EVar(a + b) )
  //a: String, b: Option[String]

  lazy val multOp = bws("+") | bws("-")
  lazy val additiveOp = bws("+") | bws("-")
  lazy val factor : Parser[AST] = (bws("-") ~ factor) ^^ EUnary | (bws("(") ~> expr <~ bws(")"))
  lazy val term = (factor ~ multOp ~ term) ^^ { EMultiplicative(_, _, _) }
  lazy val expr: Parser[AST] = (term ~ additiveOp ~ expr) ^^ EAdditive
  lazy val stmt = ((variable <~ bws("=")) ~ (expr <~ bws(";"))) ^^ Stmt
  lazy val pgm = (stmt*) ~ expr ^^ Pgm
}

object Main extends Parsers {
  def main(args: Array[String]) = {
    //val res = expr("0" * 12).toArray
    //res.sortBy(e => e.toString().length).foreach(r => println(r))
    //println(s"savedToDone   $savedToDone foundInDone  $foundInDone notFoundInDone  $notFoundInDone")
    //println(s"savedToPopped $savedToPopped foundInPopped  $foundInPopped notFoundInPopped $notFoundInPopped")
    //println(s"savedToSaved  $savedToSaved foundInSaved  $foundInSaved")
    //println(s"queuePushed   $queuePushed")

  }
}
