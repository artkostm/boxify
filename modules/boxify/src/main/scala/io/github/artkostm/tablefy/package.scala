package io.github.artkostm

import cats.syntax.monoid.*

package object tablefy:
  import io.github.artkostm.boxify.*
  import Alignment.*
  import Box.*

  /**
   * The main method to build the table
   *
   * {{{
   * scala>
   * import io.github.artkostm.tablefy.*
   * import io.github.artkostm.tablefy.Box.*
   *
   * unsafePrintBox(
   *   table(
   *     List(List.tabulate(5)(i => s"Header$i")) :::
   *       List.tabulate(5)(j => List.tabulate(5)(i => s"${i}x$j"))
   *   )
   * )
   *
   * Output:
   * +-------+-------+-------+-------+-------+
   * |Header0|Header1|Header2|Header3|Header4|
   * +-------+-------+-------+-------+-------+
   * |0x0    |1x0    |2x0    |3x0    |4x0    |
   * +-------+-------+-------+-------+-------+
   * |0x1    |1x1    |2x1    |3x1    |4x1    |
   * +-------+-------+-------+-------+-------+
   * |0x2    |1x2    |2x2    |3x2    |4x2    |
   * +-------+-------+-------+-------+-------+
   * |0x3    |1x3    |2x3    |3x3    |4x3    |
   * +-------+-------+-------+-------+-------+
   * |0x4    |1x4    |2x4    |3x4    |4x4    |
   * +-------+-------+-------+-------+-------+
   * }}}
   *
   * @return Box
   */
  def table: List[List[String]] => Box =
    rows =>
      val columns = rows.transpose
      val nrows = rows.size
      val vsep = vcat(left, ("+" + ("|+" * nrows)).map(char).toList)
      vsep |+| hcat(top, columns.map(fmtColumn).intersperse(vsep)) |+| vsep

  // todo: add table format
  def fmtColumn: List[String] => Box =
    items =>
      val width = items.map(_.length).max
      val hsep = text("-" * width)
      hsep || vcat(left, items.map(i => text(pad(width, i))).intersperse(hsep)) || hsep
