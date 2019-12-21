package com.janboerman.aoc2019.day16

import scala.io.Source

object Day16 extends App {

    val fileName = "src/main/resources/day16input.txt"
    val numbers = Source.fromFile(fileName).getLines().next().toSeq.map(_ - '0')

    val sequence: LazyList[Int] = LazyList.continually(LazyList(0, 1, 0, -1)).flatten

    //val numbers = Seq(1, 2, 3, 4, 5, 6, 7, 8)

    var iteration: Seq[Int] = numbers
    for (_ <- 0 until 100) {
        /*I dont understand why toList is required in order to avoid a StackOverflowError...*/
        iteration = LazyList.fill(numbers.size)(iteration).zipWithIndex.map({case (series, index) => process(index, series)}).toList
    }
    val result1 = iteration.take(8).mkString
    println(result1)


    def process(index: Int, sequence: Seq[Int]): Int = {
        val combineWith = getSequence(index)
        val sum = sequence.zip(combineWith).map({case (one, two) => one * two}).sum
        val digit = Math.abs(sum) % 10
        digit
    }

    def getSequence(index: Int): LazyList[Int] = {
        val repeat = index + 1
        sequence.flatMap(value => LazyList.fill(repeat)(value)).tail
    }

}
