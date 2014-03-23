package gkit.sql

sealed trait Statement
case class Select(cols: List[Column[_]], clauses: List[Clause]) extends Statement
case class Insert(tabName: String, cols: List[Column[_]]) extends Statement
case class Update(tabName: String, cols: List[Column[_]], restr: Option[Where[_]]) extends Statement
case class Delete(from: From, restr: Option[Where[_]]) extends Statement

sealed trait Clause
case class With(as: String, stmt1: Statement, stmt2: Statement) extends Clause
case class From(labels: List[String]) extends Clause
case class Where[A](exp: Exp[A]) extends Clause
case class Limit(n: Int) extends Clause
case class Offset(n: Int) extends Clause

sealed trait Exp[A]
case class Literal[A](value: A) extends Exp[A]
case class Column[A](label: String, as: String = "") extends Exp[A]
case class EQ[A](col: Column[A], lit: Literal[A]) extends Exp[A]
case class LT[A](col: Column[A], lit: Literal[A]) extends Exp[A]
case class LTE[A](col: Column[A], lit: Literal[A]) extends Exp[A]
case class GT[A](col: Column[A], lit: Literal[A]) extends Exp[A]
case class GTE[A](col: Column[A], lit: Literal[A]) extends Exp[A]
case class And[A, B](left: Exp[A], right: Exp[B]) extends Exp[(A, B)]
case class Or[A, B](left: Exp[A], right: Exp[B]) extends Exp[(A, B)]

class ColumnOps[A](col: Column[A]) {
  def ===(a: A) = EQ(col, Literal(a))
  def <(a: A) = EQ(col, Literal(a))
  def <=(a: A) = EQ(col, Literal(a))
  def >(a: A) = EQ(col, Literal(a))
  def >=(a: A) = EQ(col, Literal(a))
}

class ExpOps[A](e1: Exp[A]) {
  def &&[B](e2: Exp[B]) = And(e1, e2)
  def ||[B](e2: Exp[B]) = Or(e1, e2)
}

object showAST {

  def apply[A](a: A): String = a match {
    case s: Statement => showStmt(s)
    case c: Clause    => showClause(c)
    case e: Exp[_]    => showExp(e)
    case _            => ""
  }

  def showExp[A](e: Exp[A]): String = e match {
    case Literal(v)    => v.toString
    case Column(l, as) => if (as.isEmpty) l else s"$l as $as"
    case EQ(col, lit)  => s"${showExp(col)} = ${showExp(lit)}"
    case LT(col, lit)  => s"${showExp(col)} < ${showExp(lit)}"
    case GT(col, lit)  => s"${showExp(col)} > ${showExp(lit)}"
    case LTE(col, lit) => s"${showExp(col)} <= ${showExp(lit)}"
    case GTE(col, lit) => s"${showExp(col)} >= ${showExp(lit)}"
    case And(l, r)     => s"${showExp(l)} and ${showExp(r)}"
    case Or(l, r)      => s"${showExp(l)} or ${showExp(r)}"
  }

  def showClause(c: Clause): String = c match {
    case With(as, s1, s2) => s"with $as as (${showStmt(s1)}) ${showStmt(s2)}"
    case From(ls)         => s"""from ${ls.mkString(", ")}"""
    case Where(e)         => s"where ${showExp(e)}"
    case Limit(n)         => s"limit ${n}"
    case Offset(n)        => s"offset ${n}"
  }

  def showStmt(s: Statement): String = s match {
    case Select(cols, cls)  =>
      val l = cols.map(showExp(_)).mkString(", ")
      val r = cls.map(showClause).mkString(" ")
      s"select $l $r"
    case Insert(tname, cols)  =>
      val cs = cols.map(_.label).mkString(", ")
      val vs = cols.map(_ => "?").mkString(", ")
      s"insert into $tname ($cs) values ($vs)"
    case Update(tname, cols, restr) =>
      val se = cols.map(c => s"${c.label} = ?").mkString(", ")
      val re = restr.map(showClause).getOrElse("")
      s"update $tname set $se $re"
    case Delete(from, restr) =>
      val re = restr.map(showClause).getOrElse("")
      s"delete ${showClause(from)} $re"
  }
}
