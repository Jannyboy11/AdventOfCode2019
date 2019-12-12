package com.janboerman.aoc2019.util

sealed trait Direction
case object North extends Direction {
    override def toString(): String = "^"
}
case object South extends Direction {
    override def toString(): String = "v"
}
case object West extends Direction {
    override def toString(): String = "<"
}
case object East extends Direction {
    override def toString(): String = ">"
}