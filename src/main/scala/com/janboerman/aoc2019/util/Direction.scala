package com.janboerman.aoc2019.util

sealed trait Direction {
    def unitPoint: Point
}
case object North extends Direction {
    val unitPoint = Point(0, -1)
    override def toString(): String = "^"
}
case object South extends Direction {
    val unitPoint = Point(0, 1)
    override def toString(): String = "v"
}
case object West extends Direction {
    val unitPoint = Point(-1, 0)
    override def toString(): String = "<"
}
case object East extends Direction {
    val unitPoint = Point(1, 0)
    override def toString(): String = ">"
}