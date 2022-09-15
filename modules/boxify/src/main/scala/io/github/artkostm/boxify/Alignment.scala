package io.github.artkostm.boxify

import cats.{Eq, Show}

enum Alignment:     // derives Eq, Show:
  case AlignFirst   // Align at the top/left.
  case AlignCenter1 // Centered, biased to the top/left.
  case AlignCenter2 // Centered, biased to the bottom/right.
  case AlignLast // Align at the bottom/right.

object Alignment:
  def top: Alignment     = AlignFirst   // Align boxes along their tops.
  def bottom: Alignment  = AlignLast    // Align boxes along their bottoms.
  def left: Alignment    = AlignFirst   // Align boxes to the left.
  def right: Alignment   = AlignLast    // Align boxes to the right.
  def center1: Alignment = AlignCenter1 // Align boxes centered, but biased to the left/top in case of unequal parities.
  // Align boxes centered, but biased to the right/bottom in case of unequal parities.
  def center2: Alignment = AlignCenter2

  // Creates an @r@ x @c@ box with the contents of @bx@, aligned horizontally according to @ah@ and vertically according to @av@.
  def align: Alignment => Alignment => Int => Int => Box => Box =
    ah => av => r => c => sb => Box(r, c, Content.SubBox(ah, av, sb))

  // Creates a box of height @n@, with the contents and width of @bx@, vertically aligned according to @algn@.
  def alignVert: Alignment => Int => Box => Box =
    a => r => b => align(AlignFirst)(a)(r)(b.cols)(b)

  // Creates a box of width @n@, with the contents and height of @bx@, horizontally aligned according to @algn@.
  def alignHoriz: Alignment => Int => Box => Box =
    a => c => b => align(a)(AlignFirst)(b.rows)(c)(b)
    
  // Move a box \"up\" by putting it in a larger box with extra rows, aligned to the top.  See the disclaimer for 'moveLeft'.
  def moveUp(n: Int, b: Box): Box =
    alignVert(top)(b.rows + n)(b)

  // Move a box down by putting it in a larger box with extra rows, aligned to the bottom.  See the disclaimer for 'moveLeft'.
  def moveDown(n: Int, b: Box): Box =
    alignVert(bottom)(b.rows + n)(b)

  // Move a box left by putting it in a larger box with extra columns,
  // aligned left.  Note that the name of this function is
  // something of a white lie, as this will only result in the box
  // being moved left by the specified amount if it is already in a
  // larger right-aligned context.
  def moveLeft(n: Int, b: Box): Box =
    alignHoriz(left)(b.cols + n)(b)

  // Move a box right by putting it in a larger box with extra columns, aligned right.  See the disclaimer for 'moveLeft'.
  def moveRight(n: Int, b: Box): Box =
    alignHoriz(right)(b.cols + n)(b)
