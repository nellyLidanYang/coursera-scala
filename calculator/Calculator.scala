package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr {override def toString: String = "Literal(" + v + ")"}
final case class Ref(name: String) extends Expr {override def toString: String = "Ref(" + name + ")"}
final case class Plus(a: Expr, b: Expr) extends Expr {override def toString: String = "Plus(" + a.toString + " + " + b.toString + ")"}
final case class Minus(a: Expr, b: Expr) extends Expr {override def toString: String = "Minus(" + a.toString + " - " + b.toString + ")"}
final case class Times(a: Expr, b: Expr) extends Expr {override def toString: String = "Times(" + a.toString + " * " + b.toString + ")"}
final case class Divide(a: Expr, b: Expr) extends Expr {override def toString: String = "Divide(" + a.toString + " / " + b.toString + ")"}

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map{ case (k, v) => (k, Signal(eval(v(), namedExpressions)))}
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    println(expr)
    println(references)
    expr match {
    case Literal(v) => v
      /*
      * interesting:
      * to detect (a-> b-> c ->a)loop, there is no need to start at "a".
      * "reference - name" actually detect a loop "starts" at "b"
      * it does not matter where to start.
      * */
    case Ref(name) => eval(getReferenceExpr(name, references), references - name)
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) =>
      val b_val = eval(b, references)
      if (b_val == 0) Double.NaN
      else eval(a, references) / b_val
  }}

  def filter(expr: Expr, ref: Map[String, Signal[Expr]]): Map[String, Signal[Expr]] = ref.map{case (k, v) if(v != expr) => (k, v)}

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
