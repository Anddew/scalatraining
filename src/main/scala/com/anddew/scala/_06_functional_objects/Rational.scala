package com.anddew.scala._06_functional_objects

class Rational(n: Int, d: Int) {
  require(d != 0, "Delimiter should be non zero.")

  private val g = gcd(n.abs, d.abs)
  val numer:Int = n/g
  val denom:Int = d/g

  def this(n: Int) = this(n, 1)

  def add(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def lessThan(that:Rational) = this.numer * that.denom < that.numer * this.denom
  def max(that:Rational) = if (this.lessThan(that)) that else this

  private def gcd(x1: Int, x2: Int): Int = if (x2 == 0) x1 else gcd(x2, x1 % x2)

  override def toString = n + "/" + d

}