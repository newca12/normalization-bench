package minimized_conv

import scala.annotation.tailrec

abstract class Val //value
case class VVar(x: Long) extends Val //variable
case class VAp(t: Val, u: Val) extends Val //application of a neutral term to a value
case class VLam(t: Val => Val) extends Val //lambda abstraction

object Main extends App {

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

  //@tailrec
  def conv(d: Long, l: Val, r: Val): Boolean = {
    (l, r) match {
      case (VVar(x), VVar(y))         => x == y
      case (VAp(l1, l2), VAp(r1, r2)) => conv(d, l1, r1) && conv(d, l2, r2)
      case (VLam(l), VLam(r))         => conv(d + 1, l(VVar(d)), r(VVar(d)))
      case _                          => false
    }
  }

  def conv0(l: Val, r: Val) = conv(0, l, r)

  //conversion
  println(conv0(n2, suc(n1)))

}
