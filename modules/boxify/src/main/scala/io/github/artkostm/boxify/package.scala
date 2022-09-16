package io.github.artkostm

package object boxify:
  extension[A] (l: List[A])
    def intersperse[B >: A](sep: B): List[B] =
      intersperseIterator(l.iterator, sep).toList

  extension[A] (l: LazyList[A])
    def intersperse[B >: A](sep: B): LazyList[B] =
      LazyList.from(intersperseIterator(l.iterator, sep))

  private def intersperseIterator[A](it: Iterator[A], sep: A): Iterator[A] =
    new Iterator[A] :
      var intersperseNext = false

      def hasNext: Boolean = intersperseNext || it.hasNext

      def next(): A =
        val elem = if intersperseNext then sep else it.next()
        intersperseNext = !intersperseNext && it.hasNext
        elem

  protected inline def chunksOf[A](n: Int, l: List[A]): List[List[A]] = l.grouped(n).toList
