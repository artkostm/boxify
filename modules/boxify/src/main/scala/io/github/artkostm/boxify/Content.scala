package io.github.artkostm.boxify

import cats.Show

enum Content:// derives Show:
  case Blank                  // No content.
  case Text(inner: String)    // A raw string.
  case Row(inner: LazyList[Box]) // A row of sub-boxes.
  case Col(inner: LazyList[Box]) // A column of sub-boxes.
  case SubBox(horizontal: Alignment, vertical: Alignment, inner: Box) // A sub-box with a specified alignment.
