package minimized_quote

import scala.annotation.tailrec

sealed abstract class Tm
final case class Var(x: Long) extends Tm
final case class Ap(t: Tm, u: Tm) extends Tm
final case class Lam(t: Tm) extends Tm

sealed abstract class Val
final case class VVar(x: Long) extends Val
final case class VAp(t: Val, u: Val) extends Val
final case class VLam(t: Val => Val) extends Val

object Main extends App {

  //@tailrec
  def quote(l: Long, v: Val): Tm = {
    v match {
      case VVar(x)   => Var(l - x - 1)
      case VAp(t, u) => Ap(quote(l, t), quote(l, u))
      case VLam(f)   => Lam(quote(l + 1, f(VVar(l))))
    }
  }

  def quote0(v: Val) = quote(0, v)

  def ap(t: Val, u: Val): Val = {
    t match {
      case VLam(f) => f(u)
      case t       => VAp(t, u)
    }
  }

  // natural numbers
  // ------------------------------------------------------------
  def ap2(f: Val, t: Val, u: Val): Val = ap(ap(f, t), u)

  val n1: VLam = VLam((s: Val) => VLam((z: Val) => ap(s, z)))

  def n2: VLam = VLam((s: Val) => VLam((z: Val) => ap(s, ap(s, z))))

  def suc(n: Val) = VLam((s: Val) => VLam((z: Val) => ap(s, ap2(n, s, z))))

  def force(t: Tm): Boolean = t match {
    case Lam(_) => true
    case _      => false
  }

  //normalization
  println(quote0(n1))
  println(quote0(n2))

}
