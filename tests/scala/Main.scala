import java.io.FileWriter

import com.codecommit.gll.{Parsers, RegexParsers}

/**
 * Created by user on 23.10.2015.
 */


object GrammarNNN extends Parsers {
  def atom : Parser[String] = literal("0")
  lazy val expr: Parser[String] = expr ~ expr ~ expr ^^ { _ + _ + _ } | expr ~ expr ^^ { _ + _ } | atom
  def runNnn(n: Int) : Array[String] = {
    expr("0" * n).toArray
  }
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

  def bws[A](p: Parser[A]) = (("\s".r)*) ~> p <~ (("\s".r)*)

  lazy val digits = (("[0-9]".r)+) ^^ { _.mkString("") }
  lazy val notDigit : Parser[String] = "[a-zA-Z_]".r
  lazy val sym : Parser[String] = "[0-9a-zA-Z_]".r

  lazy val value = (digits ~ ((literal(".") ~ digits ^^ { _ + _ })?)) ^^ { (a, b) => EVal((a + b.getOrElse("")).toDouble) }
  lazy val variable : Parser[EVar] = bws(notDigit ~ (sym*)) ^^ ( (a, b) => EVar(a + b) )
  lazy val multOp = bws("*") | bws("/")
  lazy val additiveOp = bws("+") | bws("-")

  lazy val factor : Parser[AST] = value | variable | ((bws("-") ~ factor) ^^ EUnary) | (bws("(") ~> expr <~ bws(")"))
  lazy val term = (factor ~ multOp ~ term) ^^ { EMultiplicative(_, _, _) }
  lazy val expr: Parser[AST] = (term ~ additiveOp ~ expr) ^^ EAdditive
  lazy val stmt = ((variable <~ bws("=")) ~ (expr <~ bws(";"))) ^^ Stmt
  lazy val pgm = (stmt*) ~ expr ^^ Pgm
}

object Main extends Parsers {
  def warmup () = {
    val gcTimeout = 500
    val ra= GrammarNNN.runNnn(10)
    println("Warmup: " + ra.length)
    System.gc()
    Thread.sleep(gcTimeout)
  }
  def measureTime(f: Unit => Any) : (Any, Long) = {
    warmup()
    val startTime = System.nanoTime()
    val r = f()
    val estimatedTime = System.nanoTime() - startTime
    (r, estimatedTime / (1000 * 1000))
  }

  def logRecord(test: String, n: Int, time: Long, log: Option[String]) = {
    val s = test + " " + n.toString + " " + (time.toDouble / 1000.0).toString
    log match {
      case Some(log) => {
        val fw = new FileWriter(log, true)
        fw.write(s)
        fw.close()
      }
      case None => println(s)
    }
  }

  def measurePerformance(test: String, n: Option[String], log: Option[String]) = {
    val withN = (f: Int => Unit) => {
        f (n.get.toInt)
    }

    test match {
      case "nnn" => withN { n =>
        val (res, time) = measureTime ( { _ => GrammarNNN.runNnn(n) } )
        println(res)
        logRecord(test, n, time, log)
      }
    }
  }

  def main(args: Array[String]) = {
    def getArg(name: String) : Option[String] = {
      val filtered = args.filter({ s => s.startsWith(s"-$name=")}).map(s => s.substring(name.length() + 2).trim())
      if (filtered.length == 1) { Some(filtered.head) } else { None }
    }
    val log = getArg("log")
    val test = getArg("test")
    val n = getArg("n")
    test match {
      case Some(test) => { measurePerformance (test, n, log) }
      case _ => println("Usage: <pgm> -test=<Test> [-n=<N>] [-log=<Log>]")
    }
    //val res = expr("0" * 12).toArray
    //res.sortBy(e => e.toString().length).foreach(r => println(r))
    //println(s"savedToDone   $savedToDone foundInDone  $foundInDone notFoundInDone  $notFoundInDone")
    //println(s"savedToPopped $savedToPopped foundInPopped  $foundInPopped notFoundInPopped $notFoundInPopped")
    //println(s"savedToSaved  $savedToSaved foundInSaved  $foundInSaved")
    //println(s"queuePushed   $queuePushed")

  }
}
