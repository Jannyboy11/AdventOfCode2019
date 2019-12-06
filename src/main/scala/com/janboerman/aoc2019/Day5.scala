package com.janboerman.aoc2019

import java.util

import scala.io.{Source, StdIn}

object Imports {
    type Address = Int
    type OpCode = Int
    type Mode = Int
    type Memory = Array[Int]

    val Add = 1
    val Multiply = 2
    val Input = 3
    val Output = 4
    val Abort = 99

    val PositionMode = 0
    val ImmediateMode = 1
}
import Imports._

object Day5 extends App {
    val fileName = "src/main/resources/day5input.txt"
    val numbers: Memory = Source.fromFile(fileName).getLines().next().split(",").map(_.toInt)

    //val numbers = Array(1002, 4, 3, 4, 33)

    eval(numbers) //input: 1

    def eval(memory: Memory): Unit = {
        var instructionPointer = 0
        while (instructionPointer != -1) {
            instructionPointer = evalOnce(memory, instructionPointer)
        }
    }

    def evalOnce(memory: Memory, address: Address): Address = {
        val instruction = Instruction.decode(memory, address)
        instruction match {
            case Instruction(Add, List(m1, m2, 0 /*should always be position mode*/), List(o1, o2, o3)) =>
                val one = readMemory(memory, o1, m1)
                val two = readMemory(memory, o2, m2)
                val three = o3 //inconsistent with modes, it says it's always position mode, but it's always immediate mode
                val result = one + two
                memory(three) = result
                address + 4
            case Instruction(/*can't alias with @ :(*/Multiply, List(m1, m2, 0 /*should always be position mode*/), List(o1, o2, o3)) =>
                val one = readMemory(memory, o1, m1)
                val two = readMemory(memory, o2, m2)
                val three = o3 //idem
                val result = one * two
                memory(three) = result
                address + 4
            case Instruction(Input, List(0 /*should alwas be position mode..*/), List(o1)) =>
                val one = o1 //again, inconsistent... readMemory(memory, o1, m1)
                val two = StdIn.readInt()
                memory(one) = two
                address + 2
            case Instruction(Output, List(m1), List(o1)) =>
                val one = readMemory(memory, o1, m1)
                println(one)
                address + 2
            case Instruction(Abort, List(), List()) =>
                -1
        }
    }

    def readMemory(memory: Memory, operand: Int, mode: Mode): Int = {
        mode match {
            case PositionMode => memory(operand)
            case ImmediateMode => operand
        }
    }

}

object Instruction {
    def decode(memory: Memory, address: Address): Instruction = {
        val value = memory(address)
        val opCode = value % 100
        var modes = value / 100

        //determine number of operands
        val numOperands = opCode match {
            case Add | Multiply => 3
            case Input | Output => 1
            case Abort => 0
        }

        val operands = util.Arrays.copyOfRange(memory, address + 1, address + 1 + numOperands).toList

        var i = 0
        var opModes: List[Mode] = List()
        while (i < numOperands) {
            val last = modes % 10
            opModes = opModes ++ List(last)

            modes /= 10
            i += 1
        }

        Instruction(opCode, opModes, operands)
    }
}

case class Instruction(opcode: OpCode, modes: List[Mode], operands: List[Int]) {
    override def toString(): String = {
        val opcodeString = opcode match {
            case Add => "Add"
            case Multiply => "Multiply"
            case Input => "Input"
            case Output => "Output"
            case Abort => "Abort"
        }
        val modesString = modes.map {case 0 => "PositionMode"; case 1 => "ImmediateMode"}
        s"Instruction($opcodeString, $modesString, $operands)"
    }
}
