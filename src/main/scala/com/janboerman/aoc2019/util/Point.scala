package com.janboerman.aoc2019.util

object Point {
    implicit class IndexedSeqExtension[A](val seq: IndexedSeq[IndexedSeq[A]]) extends AnyVal {
        @inline def apply(point: Point): A = seq(point.y)(point.x)
    }
}

case class Point(x: Int, y: Int) {
    def add(that: Point): Point = Point(this.x + that.x, this.y + that.y)
    def subtract(that: Point): Point = Point(this.x - that.x, this.y - that.y)

    def -(that: Point): Point = subtract(that)
    def +(that: Point): Point = add(that)
}
