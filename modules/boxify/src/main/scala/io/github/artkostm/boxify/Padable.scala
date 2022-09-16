package io.github.artkostm.boxify

sealed trait Padable[A]:
  type Filler
  def reverse(a: A): A
  def splitAt(a: A, n: Int): (A, A)
  def length(a: A): Int
  def takeP(filler: Filler, n: Int, value: A): A

protected[boxify] object Padable:
  inline def apply[A: Padable]: Padable[A] = summon

  given Padable[String] with
    override type Filler = Char
    override def reverse(a: String): String                         = a.reverse
    override def splitAt(a: String, n: Int): (String, String)       = a.splitAt(n)
    override def length(a: String): Int                             = a.length
    override def takeP(filler: Char, n: Int, value: String): String =
      if n <= 0 then ""
      else if n <= value.length then value.take(n)
      else value.padTo(n, filler)

  given[A]: Padable[LazyList[A]] with
    override type Filler = A
    override def reverse(a: LazyList[A]): LazyList[A] = a.reverse
    override def splitAt(a: LazyList[A], n: Int): (LazyList[A], LazyList[A]) = a.splitAt(n)
    override def length(a: LazyList[A]): Int = a.size
    override def takeP(filler: A, n: Int, value: LazyList[A]): LazyList[A] =
      if n <= 0 then LazyList.empty
      else if n <= value.length then value.take(n)
      else value.padTo(n, filler)
