package com.janboerman.aoc2019

import scala.collection.immutable.IntMap

object Day4 extends App {

    val inputLower = 152085
    val inputUpper = 670283

//    println(checkRequirements1(111111))
//    println(checkRequirements1(223450))
//    println(checkRequirements1(123789))

//    println(checkRequirements2(112233))
//    println(checkRequirements2(123444))
//    println(checkRequirements2(111122))

    println(count(checkRequirements1))
    println(count(checkRequirements2))

    def count(predicate: Int => Option[Int]): Int = {
        var number = inputLower
        var count = 0
        while (number <= inputUpper) {
            predicate(number) match {
                case Some(continue) =>
                    //not a good number, prepare for next iteration
                    number = continue
                case None =>
                    //good number
                    count += 1

                    //prepare for next iteration
                    number += 1
            }
        }
        count
    }

    //I can probably factor out some common code between checkRequirements 1 and 2
    def checkRequirements2(number: Int): Option[Int] = {
        var pRight = 0;

        var doubles: IntMap[Int] = IntMap()
        while (pRight < 5) {
            val pLeft = pRight + 1

            val leftDigit = digit(number, pLeft)
            val rightDigit = digit(number, pRight)

            if (rightDigit < leftDigit) {
                return Some(nextSatisfyable(number, pLeft))
            } else if (leftDigit == rightDigit) {
                val oldCount: Int = doubles.applyOrElse[Int, Int](leftDigit, _ => 0)
                doubles = doubles.updated(leftDigit, oldCount + 1)
            }

            pRight += 1
        }

        if (doubles.exists({case (_, v) => v == 1})) None else Some(number + 1)
    }

    def checkRequirements1(number: Int): Option[Int] = {
        var pRight = 0;

        var double = false
        while (pRight < 5) {
            val pLeft = pRight + 1

            val leftDigit = digit(number, pLeft)
            val rightDigit = digit(number, pRight)

            if (rightDigit < leftDigit ) {
                //number does not satisfy the requirements
                //continue with next number
                return Some(nextSatisfyable(number, pLeft))
            } else if (leftDigit == rightDigit) {
                double = true
            }

            pRight += 1
        }

        if (double) None else Some(number + 1)
    }

    def nextSatisfyable(number: Int, fromPosition: Int): Int = {
        val powerTen = intPow(10, fromPosition)
        val baseNumber = number / powerTen * powerTen
        val toDigit = digit(number, fromPosition)

        var position = 0
        var power = 1
        var result = baseNumber
        while (position < fromPosition) {
            val add = toDigit * power
            result += add
            power *= 10
            position += 1
        }

        result
    }

    def digit(number: Int, position: Int): Int = {
        val powerTen = intPow(10, position)
        val shifted = number / powerTen
        val remainder = shifted % 10
        remainder
    }

    def intPow(base: Int, exp: Int): Int = {
        if (exp == 0) return 1

        var result = base
        var i = 1
        while (i < exp) {
            result *= base
            i += 1
        }

        result
    }

}
