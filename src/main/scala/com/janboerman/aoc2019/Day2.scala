package com.janboerman.aoc2019

import scala.collection.mutable
import scala.io.Source

object Day2 extends App {

    val fileName = "src/main/resources/day2input.txt"
    val numbers: mutable.ArraySeq[Int] = Source.fromFile(fileName).getLines().next().split(",").map(_.toInt)

    //val numbers: mutable.ArraySeq[Int] = mutable.ArraySeq(1,9,10,3,2,3,11,0,99,30,40,50)

    type Address = Int

    val Add = 1
    val Multiply = 2
    val Abort = 99

    def solutionOne(): Int = {
        val memory = numbers.clone()

        eval(memory)
    }

    def solutionTwo(): Int = {
        for (noun <- 0 to 99) {
            for (verb <- 0 to 99) {
                val memory = makeMemory(noun, verb)

                if (eval(memory) == 19690720) {
                    return 100 * noun + verb
                }
            }
        }

        throw new RuntimeException("Should not be reachable")
    }

    println(solutionOne())
    println(solutionTwo())

    def eval(memory: mutable.ArraySeq[Int]): Int = {
        var instructionPointer = 0
        while (instructionPointer != -1) {
            instructionPointer = evalOnce(memory, instructionPointer)
        }

        memory(0)
    }

    def makeMemory(noun: Int, verb: Int): mutable.ArraySeq[Int] = {
        val memory = numbers.clone()

        memory(1) = noun
        memory(2) = verb

        memory
    }

    def evalOnce(numbers: mutable.ArraySeq[Int], position: Address): Address = {
        val opCode = numbers(position)
        opCode match {
            case Add =>
                val (one, two, three) = readOperands(numbers, position)
                val result = numbers(one) + numbers(two)
                numbers(three) = result
                position + 4
            case Multiply =>
                val (one, two, three) = readOperands(numbers, position)
                val result = numbers(one) * numbers(two)
                numbers(three) = result
                position + 4
            case Abort =>
                -1
        }
    }

    def readOperands(numbers: mutable.ArraySeq[Int], address: Address): (Int, Int, Int) = {
        (numbers(address + 1), numbers(address + 2), numbers(address + 3))
    }
}

