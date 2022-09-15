package io.github.artkostm.boxify

import cats.arrow.{Arrow, Strong}
import cats.{Foldable, Monoid, Order, Semigroup, Show}
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.targetName
import scala.jdk.CollectionConverters.ListHasAsScala

case class Box(rows: Int, cols: Int, content: Content)

object Box:
  import Content.*

  extension (box: Box)
    // Paste two boxes together horizontally with a single intervening column of space, using a default (top) alignment.
    @targetName("plusHoriz")
    infix def <+>(r: Box): Box = hcat(Alignment.top, List(box, empty(0)(1), r))
    // Paste two boxes together vertically, using a default (left) alignment.
    @targetName("plusVert")
    infix def ||(b: Box): Box  = vcat(Alignment.left, List(box, b))
    // Paste two boxes together vertically with a single intervening row of space, using a default (left) alignment.
    @targetName("plusVertInt")
    infix def /+/(b: Box): Box = vcat(Alignment.left, List(box, empty(1)(0), b))

  given Semigroup[Box] with
    override def combine(x: Box, y: Box): Box =
      hcat(Alignment.top, List(x, y))

  given Monoid[Box] with
    override def empty: Box = nil
    override def combine(x: Box, y: Box): Box =
      hcat(Alignment.top, List(x, y))

  given Show[Box] with
    override def show(t: Box): String = render(t)

  // It is an empty box with @r@ rows and @c@ columns.
  // Useful for effecting more fine-grained positioning of other
  // boxes, by inserting empty boxes of the desired size in between
  // them.
  def empty: Int => Int => Box = r => c => Box(r, c, Blank)

  // The null box, which has no content and no size.  It is quite useless.
  def nil: Box = empty(0)(0)

  // A @1x1@ box containing a single character.
  def char: Char => Box = c => Box(1, 1, Text(String.valueOf(c)))

  def unsafeLine: String => Box = t => Box(1, t.length, Text(t))

  // A (@1 x len@) box containing a string length @len@. The newline character are filtered out.
  def line: String => Box =
    t => unsafeLine(t.replaceAll("[\n\r]", ""))

  // A box containing lines of text. May be empty.
  def text: String => Box =
    s => vcat(Alignment.left, s.lines().toList.asScala.map(unsafeLine).toList)

  // Glue a list of boxes together horizontally, with the given alignment.
  def hcat[F[_]: Foldable](a: Alignment, bs: F[Box]): Box =
    val (w, h) = sumMax[F, Box, Int, Int](_.cols, 0, _.rows, bs)
    Box(h, w, Row(Foldable[F].toList(bs).map(b => Alignment.alignVert(a)(h)(b))))

  // Glue a list of boxes together vertically, with the given alignment.
  def vcat[F[_]: Foldable](a: Alignment, bs: F[Box]): Box =
    val (h, w) = sumMax[F, Box, Int, Int](_.rows, 0, _.cols, bs)
    Box(h, w, Col(Foldable[F].toList(bs).map(b => Alignment.alignHoriz(a)(w)(b))))

  // It lays out the boxes @bs@ with a copy of @p@ interspersed between each.
  def punctuateV[F[_]: Foldable](a: Alignment, p: Box, bs: F[Box]): Box =
    vcat[List](a, Foldable[F].toList(bs).intersperse(p))

  // It lays out the boxes @bs@ with a copy of @p@ interspersed between each.
  def punctuateH[F[_]: Foldable](a: Alignment, p: Box, bs: F[Box]): Box =
    hcat[List](a, Foldable[F].toList(bs).intersperse(p))

  // It lays out @bs@ vertically with alignment @a@, with @sep@ amount of space in between each.
  def vsep[F[_]: Foldable]: Int => Alignment => F[Box] => Box =
    sep => a => bs => punctuateV(a, empty(sep)(0), bs)

  // It lays out @bs@ horizontally with alignment @a@, with @sep@ amount of space in between each.
  def hsep[F[_]: Foldable]: Int => Alignment => F[Box] => Box =
    sep => a => bs => punctuateH(a, empty(0)(sep), bs)

  // It makes a box of height @n@ with the text @s@ aligned according to @a@.
  def mkParaBox: Alignment => Int => List[String] => Box =
    a => n => ls => Alignment.alignVert(Alignment.top)(n)(vcat(a, ls.map(text)))

  // Render a 'Box' as a String, suitable for writing to the screen or a file. Also, strips trailing whitespace.
  def render: Box => String =
    renderBox(_).map(_.stripTrailing()).mkString("\n")

  // Like 'render' but preserves end-of-line whitespace.
  def renderWithSpaces: Box => String =
    renderBox(_).mkString("\n")

  // Render a box as a list of lines.
  def renderBox: Box => List[String] =
    case Box(r, c, Blank)             => resizeBox(r, c)(List(""))
    case Box(r, c, Text(t))           => resizeBox(r, c)(List(t))
    case Box(r, c, Row(bs))           =>
      resizeBox(r, c) {
        bs.map(box => renderBoxWithRows(r)(box))
          .to(LazyList)
          .foldRight(LazyList.continually[String]("")) { case (items, l) =>
            items.to(LazyList).lazyZip(l).map(_ + _)
          }
          .toList
      }
    case Box(r, c, Col(bs))           => resizeBox(r, c)(bs.flatMap(box => renderBoxWithCols(c)(box)))
    case Box(r, c, SubBox(ha, va, b)) => resizeBoxAligned(r)(c)(ha)(va)(renderBox(b))

  // A convenience function for rendering a box to stdout.
  def printBox[F[_]: Console]: Box => F[Unit] = Console[F].println

  def unsafePrintBox: Box => Unit = b => println(render(b))

  // Render a box as a list of lines, using a given number of rows.
  private def renderBoxWithRows: Int => Box => List[String] =
    r => b => renderBox(b.copy(rows = r))

  // Render a box as a list of lines, using a given number of columns.
  private def renderBoxWithCols: Int => Box => List[String] =
    c => b => renderBox(b.copy(cols = c))

  private inline def blanks(n: Int): String = " " * n

  // Resize a rendered list of lines.
  private def resizeBox(using PLS: Padable[List[String]], PS: Padable[String])(r: Int, c: Int): List[String] => List[String] =
    s => PLS.takeP(blanks(c).asInstanceOf[PLS.Filler], r, s.map(PS.takeP(' '.asInstanceOf[PS.Filler], c, _)))

  // It is like Padable' 'takeP', but with alignment.  That is, we imagine a copy of @xs@ extended infinitely on both sides with
  // copies of @a@, and a window of size @n@ placed so that @xs@ has the specified alignment within the window; @takePA algn a n xs@
  // returns the contents of this window.
  private def takePA[A](using M: Monoid[A], P: Padable[A]): Alignment => P.Filler => Int => A => A =
    c => b => n0 => l =>
      val numRev: (Alignment, Int) => Int =
        case (Alignment.AlignFirst, _)   => 0
        case (Alignment.AlignLast, n)    => n
        case (Alignment.AlignCenter1, n) => (n + 1) / 2
        case (Alignment.AlignCenter2, n) => n / 2

      val numFwd: (Alignment, Int) => Int =
        case (Alignment.AlignFirst, n)   => n
        case (Alignment.AlignLast, _)    => 0
        case (Alignment.AlignCenter1, n) => n / 2
        case (Alignment.AlignCenter2, n) => (n + 1) / 2

      val split: A => (A, A) =
        t => Strong[Function1].first[A, A, A](P.reverse)(P.splitAt(t, numRev(c, P.length(t))))

      val both = (P.takeP(b, numRev(c, n0), _)) *** (P.takeP(b, numFwd(c, n0), _))
      val res  = Strong[Function1].first[A, A, A](P.reverse)(both(split(l)))

      val glue: (A, A) => A = _ |+| _

      glue.tupled(res)

  // Resize a rendered list of lines, using given alignments.
  private def resizeBoxAligned: Int => Int => Alignment => Alignment => List[String] => List[String] =
    r => c => ha => va => l =>
      takePA[List[String]](va)(blanks(c))(r)(l.map(s => takePA[String](ha)(' ')(c)(s)))

  // Calculate a sum and a maximum over a list in one pass. If the list is
  // empty, the maximum is reported as the given default.
  private def sumMax[F[_]: Foldable, A, N, B](using N: Numeric[N], O: Order[B])
                                             (f: A => N, defaultMax: B, g: A => B, as: F[A]): (N, B) =
    Foldable[F].foldLeft(as, (Numeric[N].zero, defaultMax)) { case ((n, b), a) => (N.plus(f(a), n), O.max(g(a), b)) }
