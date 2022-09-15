package io.github.artkostm

package object boxify:
  extension[A] (l: List[A])
    def intersperse[B >: A](sep: B): List[B] =
      val it = l.iterator
      val res: Iterator[B] = new Iterator[B]:
        var intersperseNext = false
        def hasNext: Boolean = intersperseNext || it.hasNext
        def next(): B =
          val elem = if intersperseNext then sep else it.next()
          intersperseNext = !intersperseNext && it.hasNext
          elem

      res.toList

  protected inline def chunksOf[A](n: Int, l: List[A]): List[List[A]] = l.grouped(n).toList

  protected def pad(width: Int, x: String): String = x.padTo(width - x.length, ' ')
