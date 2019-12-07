package com.janboerman.aoc2019.day7

import java.util

import scala.collection.mutable
import scala.io.Source

object Imports {
    type Address = Int
    type OpCode = Int
    type Mode = Int
    type Memory = IndexedSeq[Int]
    type Signal = Int
    type Phase = Int

    val Add = 1
    val Multiply = 2
    val Input = 3
    val Output = 4
    val JumpIfTrue = 5
    val JumpIfFalse = 6
    val LessThan = 7
    val Equals = 8
    val Abort = 99

    val PositionMode = 0
    val ImmediateMode = 1
}
import Imports._

sealed trait Control
case object Continue extends Control
case object NextAmp extends Control
case object Stop extends Control

sealed trait ExecutionMode
case object SequentialMode extends ExecutionMode
case object FeedbackMode extends ExecutionMode

object Day7 extends App {
    val fileName = "src/main/resources/day7input.txt"
    val numbers: Memory = Source.fromFile(fileName).getLines().next().split(",").map(_.toInt).toIndexedSeq

    def newMemory(): Memory = {
        numbers
        //IndexedSeq(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
        //IndexedSeq(3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0)
        //IndexedSeq(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)

        //IndexedSeq(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    }

    //println(evalSequentially(List(4,3,2,1,0)))
    //println(evalSequentially(List(0,1,2,3,4)))
    //println(evalSequentially(List(1,0,4,3,2)))


    val result1 = (0 to 4).permutations.map(settings => evalSequentially(settings)).max
    println(result1)


    //println(evalFeedback(IndexedSeq(9,8,7,6,5)))

    def evalFeedback(phaseSettings: IndexedSeq[Phase]): Signal = {
        val amplifiers = mutable.HashMap[Int, Amplifier]()
        var signal = 0
        var control: Control = Continue

        while (control != Stop) {
            var id = 0
            while (id < 5) {
                var phase = phaseSettings(id)

                //println(s"====================================== MOVING TO AMPLIFIER $id ======================================")
                var amplifier = amplifiers.getOrElseUpdate(id, Amplifier(0, newMemory(), phase, 0, signal, control, FeedbackMode))
                //amplifier.address = 0

                amplifier = evalAmplifier(amplifier)
                signal = amplifier.signal
                control = amplifier.control
                //println(s"UPDATED SIGNAL $signal")
                //println(s"UPDATED CONTROL $control")
                if (control == Stop) return signal

                amplifiers.put(id, amplifier)

                id += 1
            }
        }

        signal
    }

    def evalSequentially(phaseSettings: Iterable[Phase]): Signal = {
        var signal = 0
        for (phase <- phaseSettings) {
            var amplifier = Amplifier(address = 0, newMemory(), phase = phase, inputCounter = 0, signal = signal, Continue, SequentialMode)
            amplifier = evalAmplifier(amplifier)
            signal = amplifier.signal
        }
        signal
    }

    def evalAmplifier(amplifier: Amplifier): Amplifier = {
        var amp = amplifier
        while (amp.control == Continue) {
            amp = stepAmplifier(amp)
        }
        amp
    }

    def stepAmplifier(amplifier: Amplifier): Amplifier = {
        import amplifier._

        val instruction = Instruction.decode(memory, address)

        println("====================================== AMPLIFIER ======================================")
        println(amplifier)
        println(instruction)
        println()


        instruction match {
            case Instruction(Add, Seq(m1, m2, 0), Seq(o1, o2, o3)) =>
                val one = readMemory(memory, o1, m1)
                val two = readMemory(memory, o2, m2)
                val three = o3
                val result = one + two
                Amplifier(address + 4, memory.updated(three, result), phase, inputCounter, signal, Continue, executionMode)
            case Instruction(Multiply, Seq(m1, m2, 0), Seq(o1, o2, o3)) =>
                val one = readMemory(memory, o1, m1)
                val two = readMemory(memory, o2, m2)
                val three = o3 //idem
                val result = one * two
                Amplifier(address + 4, memory.updated(three, result), phase, inputCounter, signal, Continue, executionMode)
            case Instruction(Input, Seq(0), Seq(o1)) =>
                val one = o1
                val two = inputCounter match {
                    case 0 => phase
                    case _ => signal
                }
                Amplifier(address + 2, memory.updated(one, two), phase, inputCounter + 1, signal, Continue, executionMode)
            case Instruction(Output, Seq(m1), Seq(o1)) =>
                val one = readMemory(memory, o1, m1)
                val control = if (amplifier.executionMode == SequentialMode) Continue else NextAmp
                Amplifier(address + 2, memory, phase, inputCounter, signal = one, control, executionMode)
            case Instruction(JumpIfTrue, Seq(m1, m2), Seq(o1, o2)) =>
                val one = readMemory(memory, o1, m1)
                val two = readMemory(memory, o2, m2)
                Amplifier(if (one != 0) two else address + 3, memory, phase, inputCounter, signal, Continue, executionMode)
            case Instruction(JumpIfFalse, Seq(m1, m2), Seq(o1, o2)) =>
                val one = readMemory(memory, o1, m1)
                val two = readMemory(memory, o2, m2)
                Amplifier(if (one == 0) two else address + 3, memory, phase, inputCounter, signal, Continue, executionMode)
            case Instruction(LessThan, Seq(m1, m2, 0), Seq(o1, o2, o3)) =>
                val one = readMemory(memory, o1, m1)
                val two = readMemory(memory, o2, m2)
                val three = o3
                val result = if (one < two) 1 else 0
                Amplifier(address + 4, memory.updated(three, result), phase, inputCounter, signal, Continue, executionMode)
            case Instruction(Equals, Seq(m1, m2, 0), Seq(o1, o2, o3)) =>
                val one = readMemory(memory, o1, m1)
                val two = readMemory(memory, o2, m2)
                val three = o3
                val result = if (one == two) 1 else 0
                Amplifier(address + 4, memory.updated(three, result), phase, inputCounter, signal, Continue, executionMode)
            case Instruction(Abort, Seq(), Seq()) =>
                val control = if (executionMode == SequentialMode) NextAmp else Stop
                Amplifier(address, memory, phase, inputCounter, signal, control, executionMode)
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
            case Add | Multiply | LessThan | Equals => 3
            case JumpIfTrue | JumpIfFalse => 2
            case Input | Output => 1
            case Abort => 0
        }

        val operands = memory.slice(address + 1, address + 1 + numOperands)

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

case class Instruction(opcode: OpCode, modes: Seq[Mode], operands: Seq[Int]) {
    override def toString(): String = {
        val opcodeString = opcode match {
            case Add => "Add"
            case Multiply => "Multiply"
            case Input => "Input"
            case Output => "Output"
            case JumpIfFalse => "JumpIfFalse"
            case JumpIfTrue => "JumpIfTrue"
            case LessThan => "LessThan"
            case Equals => "Equals"
            case Abort => "Abort"
        }
        val modesString = modes.map {case 0 => "PositionMode"; case 1 => "ImmediateMode"}
        s"Instruction($opcodeString, $modesString, $operands)"
    }
}

case class Amplifier(var address: Address, memory: Memory, var phase: Phase, inputCounter: Int, signal: Signal, control: Control, executionMode: ExecutionMode) {
    override def toString(): String = {
        s"Amplifier\n(address = $address\n,memory = $memory\n,phase = $phase\n,inputCounter = $inputCounter\n,signal = $signal\n,control = $control\n,executionMode = $executionMode\n)"
    }
}

