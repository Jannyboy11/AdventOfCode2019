package com.janboerman.aoc2019.day1

import scala.io.Source

object Day1 extends App {

    val fileName = "src/main/resources/day1input.txt"
    val result1 = Source.fromFile(fileName).getLines().map(_.toDouble).map(requiredFuel).sum
    val result2 = Source.fromFile(fileName).getLines().map(_.toDouble).map(recursiveFuel).sum

//    println(requiredFuel(12D))
//    println(requiredFuel(14D))
//    println(requiredFuel(1969D))
//    println(requiredFuel(100756D))

//    println(recursiveFuel(14D))
//    println(recursiveFuel(1969D))
//    println(recursiveFuel(100756D));

    println(result1)
    println(result2)

    def requiredFuel(componentMass: Double): Double = {
        val dividedByThree = componentMass / 3
        val floored = Math.floor(dividedByThree)
        val subtracted = floored - 2D
        subtracted
    }

    def recursiveFuel(componentMass: Double): Double = {
        val fuel = requiredFuel(componentMass)
        if (fuel < 0) 0D else fuel + recursiveFuel(fuel)
    }

}
