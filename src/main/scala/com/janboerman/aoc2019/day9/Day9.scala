package com.janboerman.aoc2019.day9

import scala.io.Source

object Imports {
    type MemoryValue = BigInt
    type Address = Int
    type OpCode = Int
    type Mode = Int
    type Memory = IndexedSeq[MemoryValue]
    type Signal = MemoryValue
    type Phase = MemoryValue

    val Add = 1
    val Multiply = 2
    val Input = 3
    val Output = 4
    val JumpIfTrue = 5
    val JumpIfFalse = 6
    val LessThan = 7
    val Equals = 8
    val RelativeOffset = 9
    val Abort = 99

    val PositionMode = 0
    val ImmediateMode = 1
    val RelativeMode = 2

    implicit class StringExtension(private val value: String) extends AnyVal {
        @inline def toMemoryValue: MemoryValue = new BigInt(new java.math.BigInteger(value))
    }
    implicit class LongExtension(private val value: Long) extends AnyVal {
        @inline def toMemoryValue: MemoryValue = BigInt(value)
    }
    implicit class IntExtension(private val value: Int) extends AnyVal {
        @inline def toMemoryValue: MemoryValue = BigInt(value)
    }
}
import Imports._

sealed trait Control
case object Continue extends Control
case object NextCpu extends Control
case object Stop extends Control

sealed trait ExecutionMode
case object SequentialMode extends ExecutionMode
case object FeedbackMode extends ExecutionMode

object Day9 extends App {
    val fileName = "src/main/resources/day9input.txt"
    val numbers: Memory = Source.fromFile(fileName).getLines().next().split(",").map(_.toMemoryValue).toIndexedSeq

    def newMemory: Memory = {
        numbers

        //IndexedSeq(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(_.toMemoryValue)
        //IndexedSeq(1102,34915192,34915192,7,4,7,99,0).map(_.toMemoryValue)
        //IndexedSeq(104L,1125899906842624L,99L).map(_.toMemoryValue)
    }

    val computer = Computer(newMemory, 1L, SequentialMode)
    val result1 = runComputer(computer)
    println(result1.signal)

    def runComputer(computer: Computer): Computer = {
        var cpu = computer
        while (cpu.control == Continue) {
            cpu = stepComputer(cpu)
        }
        cpu
    }

    def stepComputer(computer: Computer): Computer = {
        import computer._

        val instruction = Instruction.decode(memory, instructionPointer)
        val nextAddress = instructionPointer + instruction.operands.size + 1

//        println(computer)
//        println(instruction)
//        println()

        instruction match {
            case Instruction(Add, Seq(m1, m2, m3), Seq(o1, o2, o3)) =>
                val (mem1, one) = readOperand(memory, o1, m1, relativeBase)
                val (mem2, two) = readOperand(mem1, o2, m2, relativeBase)
                val position = writePosition(o3, m3, relativeBase)
                var newMemory = resizeMemory(mem2, position)
                val result = one + two
                newMemory = newMemory.updated(position, result)
                Computer(nextAddress, newMemory, relativeBase, inputCounter, signal, Continue, executionMode)
            case Instruction(Multiply, Seq(m1, m2, m3), Seq(o1, o2, o3)) =>
                val (mem1, one) = readOperand(memory, o1, m1, relativeBase)
                val (mem2, two) = readOperand(mem1, o2, m2, relativeBase)
                val position = writePosition(o3, m3, relativeBase)
                var newMemory = resizeMemory(mem2, position)
                val result = one * two
                newMemory = newMemory.updated(position, result)
                Computer(nextAddress, newMemory, relativeBase, inputCounter, signal, Continue, executionMode)
            case Instruction(Input, Seq(m1), Seq(o1)) =>
                val position = writePosition(o1, m1, relativeBase)
                val newMemory = memory.updated(position, signal)
                Computer(nextAddress, newMemory, relativeBase, inputCounter + 1, signal, Continue, executionMode)
            case Instruction(Output, Seq(m1), Seq(o1)) =>
                val (_, one) = readOperand(memory, o1, m1, relativeBase)
                val ctrl = if (computer.executionMode == SequentialMode) Continue else NextCpu
                Computer(nextAddress, memory, relativeBase, inputCounter, signal = one, control = ctrl, executionMode)
            case Instruction(JumpIfTrue, Seq(m1, m2), Seq(o1, o2)) =>
                val (_, one) = readOperand(memory, o1, m1, relativeBase)
                val (_, two) = readOperand(memory, o2, m2, relativeBase)
                Computer(if (one != 0) two.toInt else nextAddress, memory, relativeBase, inputCounter, signal, Continue, executionMode)
            case Instruction(JumpIfFalse, Seq(m1, m2), Seq(o1, o2)) =>
                val (_, one) = readOperand(memory, o1, m1, relativeBase)
                val (_, two) = readOperand(memory, o2, m2, relativeBase)
                Computer(if (one == 0) two.toInt else nextAddress, memory, relativeBase, inputCounter, signal, Continue, executionMode)
            case Instruction(LessThan, Seq(m1, m2, m3), Seq(o1, o2, o3)) =>
                val (mem1, one) = readOperand(memory, o1, m1, relativeBase)
                val (mem2, two) = readOperand(mem1, o2, m2, relativeBase)
                val position = writePosition(o3, m3, relativeBase)
                var newMemory = resizeMemory(mem2, position)
                val result = if (one < two) 1 else 0
                newMemory = newMemory.updated(position, result)
                Computer(nextAddress, newMemory, relativeBase, inputCounter, signal, Continue, executionMode)
            case Instruction(Equals, Seq(m1, m2, m3), Seq(o1, o2, o3)) =>
                val (mem1, one) = readOperand(memory, o1, m1, relativeBase)
                val (mem2, two) = readOperand(mem1, o2, m2, relativeBase)
                val position = writePosition(o3, m3, relativeBase)
                var newMemory = resizeMemory(mem2, position)
                val result = if (one == two) 1 else 0
                newMemory = newMemory.updated(position, result)
                Computer(nextAddress, newMemory, relativeBase, inputCounter, signal, Continue, executionMode)
            case Instruction(RelativeOffset, Seq(m1), Seq(o1)) =>
                val (newMemory, one) = readOperand(memory, o1, m1, relativeBase)
                val newRelativeBase = relativeBase + one.toInt
                Computer(nextAddress, newMemory, newRelativeBase, inputCounter, signal, Continue, executionMode)
            case Instruction(Abort, Seq(), Seq()) =>
                val control = if (executionMode == SequentialMode) NextCpu else Stop
                Computer(nextAddress, memory, relativeBase, inputCounter, signal, control = control, executionMode)

            case erroneousInstruction =>
                throw new RuntimeException(s"Erroneous instruction: $erroneousInstruction")
        }
    }

    def writePosition(offset: MemoryValue, mode: Mode, relativeBase: Address): Address = mode match {
        case PositionMode => offset.intValue
        case RelativeMode => relativeBase + offset.intValue
        case ImmediateMode => throw new RuntimeException("Cannot write to immediate value!")
    }

    def readOperand(memory: Memory, operand: MemoryValue, mode: Mode, relativeBase: Address): (Memory, MemoryValue) = {
        var mem = memory
        val value = mode match {
            case PositionMode =>
                val address = operand.intValue
                mem = resizeMemory(mem, address)
                mem(address)
            case ImmediateMode => operand
            case RelativeMode =>
                val address = relativeBase + operand.intValue
                mem = resizeMemory(mem, address)
                mem(address)
        }
        (mem, value)
    }

    def resizeMemory(memory: Memory, toIndex: Address): Memory = {
        if (toIndex < memory.size) memory else memory ++ IndexedSeq.fill(toIndex - memory.size + 1)(0L)
    }

}

object Instruction {
    def decode(memory: Memory, address: Address): Instruction = {
        val value = memory(address).toInt
        val opCode = value % 100
        var modes = value / 100

        //determine number of operands
        val numOperands = opCode match {
            case Add | Multiply | LessThan | Equals => 3
            case JumpIfTrue | JumpIfFalse => 2
            case Input | Output | RelativeOffset => 1
            case Abort => 0
            case _ => throw new RuntimeException(s"Erroneous opcode: $opCode")
        }

        val operands = memory.slice(address + 1, address + 1 + numOperands)

        var i = 0
        var operandModes: List[Mode] = List()
        while (i < numOperands) {
            val last = modes % 10
            operandModes = operandModes ++ List(last)

            modes /= 10
            i += 1
        }

        Instruction(opCode.toInt, operandModes, operands)
    }
}

case class Instruction(opcode: OpCode, modes: Seq[Mode], operands: Seq[MemoryValue]) {
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
            case RelativeOffset => "RelativeOffset"
            case Abort => "Abort"
        }
        val modesString = modes.map {
            case PositionMode => "PositionMode"
            case ImmediateMode => "ImmediateMode"
            case RelativeMode => "RelativeMode"
        }

        s"Instruction($opcodeString, $modesString, $operands)"
    }
}

object Computer {
    def apply(memory: Memory, input: Signal, executionMode: ExecutionMode): Computer =
        new Computer(0, memory, 0, 0, input, Continue, executionMode)
}
case class Computer(instructionPointer: Address,
                    memory: Memory,
                    relativeBase: Address,
                    inputCounter: Int,
                    signal: Signal,
                    control: Control,
                    executionMode: ExecutionMode) {

    override def toString: String = {
        s"Computer\n(instructionPointer=$instructionPointer\n,memory=$memory\n,relativeBase=$relativeBase\n,inputCounter=$inputCounter\n,signal=$signal\n,control=$control\n,executionMode=$executionMode\n)"
    }
}

