package com.janboerman.aoc2019.util

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.IntMap
import scala.collection.{SeqFactory, mutable}

//WIP!

@Deprecated
object IntMapSeq {
    def empty[A]: IntMapSeq[A] = new IntMapSeq(IntMap())
    def from[A](elements: IterableOnce[A]): IntMapSeq[A] = elements match {
        case ims: IntMapSeq[A] => ims
        case is: IndexedSeq[A] =>
            val intMap = IntMap.from((0 until is.size).map(i => (i, is(i))))
            new IntMapSeq(intMap)
        case _ => new IntMapSeqBuilder[A].addAll(elements).result()
    }
    def apply[A](elements: A*): IntMapSeq[A] = elements match {
        case ims: IntMapSeq[A] => ims
        case is: IndexedSeq[A] =>
            val intMap = IntMap.from((0 until is.size).map(i => (i, is(i))))
            new IntMapSeq(intMap)
        case _ =>
            val intMap: IntMap[A] = IntMap.from(elements.zipWithIndex.map({case (one, two) => (two, one)}))
            new IntMapSeq(intMap)
    }

    def unapply[A](arg: IntMapSeq[A]): Option[(IntMap[A], Int => A)] = Some(arg.map, arg.fallback)

    private val defaultFallback = (i: Int) => throw new NoSuchElementException(s"No element at index $i")

    implicit class IterableExtensions[A](val iterable: Iterable[A]) extends AnyVal {
        @inline def toIntMapSeq: IntMapSeq[A] = from(iterable)
    }
    implicit class IteratorExtensions[A](val iterator: Iterator[A]) extends AnyVal {
        @inline def toIntMapSeq: IntMapSeq[A] = from(iterator)
    }
    implicit class ArrayExtensions[A](val array: Array[A]) extends AnyVal {
        @inline def toIntMapSeq: IntMapSeq[A] = apply(array: _*)
    }
}

@Deprecated
class IntMapSeq[+A](private val map: IntMap[A], private val fallback: Int => A) extends IndexedSeq[A] {
    def this(map: IntMap[A], defaultValue: => A) = this(map, _ => defaultValue)
    def this(map: IntMap[A]) = this(map, IntMapSeq.defaultFallback)

    def withDefault[B >: A](supplier: Int => B): IntMapSeq[B] = new IntMapSeq(map, supplier)

    override def apply(i: Int): A = map.getOrElse[A](i, fallback.apply(i))
    override def length: Int = map.lastKey + 1
    override def isDefinedAt(idx: Int): Boolean = super.isDefinedAt(idx) || fallback == IntMapSeq.defaultFallback

    override def appended[B >: A](last: B): IntMapSeq[B] = new IntMapSeq(map.updated(length, last), fallback)
    override def prepended[B >: A](head: B): IntMapSeq[B] = new IntMapSeq(map.map({case (k, v) => (k+1, v)}).updated(0, head))

    override def appendedAll[B >: A](suffix: IterableOnce[B]): IndexedSeq[B] = suffix match {
        case IntMapSeq(m, fallback) => new IntMapSeq(map ++ m.map({case (k, v) => (k + this.length, v)}), fallback)
        case that@IntMapSeq(m, f) => new IntMapSeq(map ++ m.map({case (k, v) => (k + this.length, v)}),
            {
                val thisLength = this.length
                val thatLength = that.length

                (idx: Int) => {
                    if (idx < 0) throw new IndexOutOfBoundsException(idx)
                    else if (idx > thisLength + thatLength) throw new IndexOutOfBoundsException(idx)
                    else if (idx < thisLength) fallback(idx)
                    else f(idx)
                }
            })
        case _ => super.appendedAll(suffix)
    }

    override def prependedAll[B >: A](prefix: IterableOnce[B]): IndexedSeq[B] = prefix match {
        case that@IntMapSeq(m, fallback) => new IntMapSeq(m ++ map.map({case (k, v) => (k + that.length, v)}), fallback)
        case that@IntMapSeq(m, f) => new IntMapSeq(m ++ map.map({case (k, v) => (k + that.length, v)}), {
            val thisLength = this.length
            val thatLength = that.length

            (idx: Int) => {
                if (idx < 0) throw new IndexOutOfBoundsException(idx)
                else if (idx > thisLength + thatLength) throw new IndexOutOfBoundsException(idx)
                else if (idx < thatLength) f(idx)
                else fallback(idx)
            }
        })
        case _ => super.prependedAll(prefix)
    }



    override def slice(from: Int, to: Int): IntMapSeq[A] = new IntMapSeq(map.slice(from, to), fallback)

    override def iterableFactory: SeqFactory[IntMapSeq] = new IntMapSeqFactory
    override def newSpecificBuilder: mutable.Builder[A @uncheckedVariance, IntMapSeq[A @uncheckedVariance]] = new IntMapSeqBuilder[A]

    override def className: String = "IntMapSeq"
    override def toString(): String = super[IndexedSeq].toString()
}

@Deprecated
class IntMapSeqFactory extends SeqFactory[IntMapSeq] {
    override def from[A](source: IterableOnce[A]): IntMapSeq[A] = IntMapSeq.from(source)
    override def empty[A]: IntMapSeq[A] = IntMapSeq()
    override def newBuilder[A]: mutable.Builder[A, IntMapSeq[A]] = new IntMapSeqBuilder[A]
}

@Deprecated
class IntMapSeqBuilder[A] extends mutable.Builder[A @uncheckedVariance, IntMapSeq[A @uncheckedVariance]] {
    private val mapBuilder = IntMap.newBuilder[A]
    var idx = 0

    override def clear(): Unit = mapBuilder.clear()
    idx = 0
    override def result(): IntMapSeq[A] = new IntMapSeq(mapBuilder.result())
    override def addOne(elem: A): this.type = {
        mapBuilder.addOne(idx, elem)
        idx += 1
        this
    }
}