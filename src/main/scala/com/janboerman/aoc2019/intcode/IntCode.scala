package com.janboerman.aoc2019.intcode

object Types {
    type MemoryValue = BigInt
    type Address = Int
    type OpCode = Int
    type OperandMode = Int
    type Memory = IndexedSeq[MemoryValue]
    trait Input[CTX] extends (CTX => (Input[CTX], CTX, MemoryValue))
    trait Output[CTX] extends ((MemoryValue, CTX) => (Output[CTX], CTX))

    private val NoInput: Input[Any] = (context: Any) => (NoInput, context, ???)
    private val NoOutput: Output[Any] = (memValue: MemoryValue, context: Any) => (NoOutput, context)

    def dummyInput[A]: Input[A] = NoInput.asInstanceOf[Input[A]]
    def dummyOutput[A]: Output[A] = NoOutput.asInstanceOf[Output[A]]
}

object Opcodes {
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
}

object OperandModes {
    val PositionMode = 0
    val ImmediateMode = 1
    val RelativeMode = 2
}

import Types._
import Opcodes._
import OperandModes._

sealed trait Control
case object Continue extends Control
case object Stop extends Control

object Computer {
    def apply[CTX](memory: Memory, input: Input[CTX], output: Output[CTX]): Computer[CTX] =
        new Computer[CTX](0, memory, 0, input, output, Continue)
}

case class Computer[CTX] private(instructionPointer: Address,
                                 memory: Memory,
                                 relativeBase: Address,
                                 fInput: Input[CTX],
                                 fOutput: Output[CTX],
                                 control: Control) {

    def step(context: CTX): (Computer[CTX], CTX) = {
        def readPosition(memory: Memory, memoryValue: MemoryValue, mode: OperandMode): (Memory, Address) = {
            mode match {
                case PositionMode =>
                    val address = memoryValue.intValue
                    val newMem = resizeMemory(memory, address)
                    (newMem, address)
                case RelativeMode =>
                    val address = relativeBase + memoryValue.intValue
                    val newMem = resizeMemory(memory, address)
                    (newMem, address)
                case ImmediateMode => throw new RuntimeException("Cannot write to immediate value!")
            }
        }

        def readOperand(memory: Memory, memoryValue: MemoryValue, mode: OperandMode): (Memory, MemoryValue) = {
            mode match {
                case PositionMode =>
                    val address = memoryValue.intValue
                    val newMem = resizeMemory(memory, address)
                    (newMem, newMem(address))
                case ImmediateMode => (memory, memoryValue)
                case RelativeMode =>
                    val address = relativeBase + memoryValue.intValue
                    val newMem = resizeMemory(memory, address)
                    (newMem, newMem(address))
            }
        }

        def resizeMemory(mem: Memory, toAddress: Address): Memory = mem.padTo(toAddress + 1, 0L)
        def writeToMemory(mem: Memory, position: Address, value: MemoryValue): Memory = resizeMemory(mem, position).updated(position, value)

        //println(memory)
        val instruction = Instruction.decode(memory, instructionPointer)
        val nextAddress = instructionPointer + instruction.operands.size + 1
        //println(s"Instruction at $instructionPointer = $instruction")

        instruction match {
            case Instruction(opCode@(Add | Multiply | LessThan | Equals), Seq(m1, m2, m3), Seq(o1, o2, o3)) =>
                val (mem1, one) = readOperand(memory, o1, m1)
                val (mem2, two) = readOperand(mem1, o2, m2)
                val (mem3, position) = readPosition(mem2, o3, m3)
                val operation: (MemoryValue, MemoryValue) => MemoryValue = opCode match {
                    case Add => (_ + _)
                    case Multiply => (_ * _)
                    case LessThan => (one, two) => if (one < two) 1L else 0L
                    case Equals => (one, two) => if (one == two) 1L else 0L
                }
                val result = operation(one, two)
                val newMemory = writeToMemory(mem3, position, result)
                (new Computer(nextAddress, newMemory, relativeBase, fInput, fOutput, Continue), context)
            case Instruction(Input, Seq(m1), Seq(o1)) =>
                val (mem1, position) = readPosition(memory, o1, m1)
                val (newInputFunction, newContext, inputValue) = fInput(context)
                val newMemory = writeToMemory(mem1, position, inputValue)
                (new Computer(nextAddress, newMemory, relativeBase, newInputFunction, fOutput, Continue), newContext)
            case Instruction(Output, Seq(m1), Seq(o1)) =>
                val (mem1, one) = readOperand(memory, o1, m1)
                val (newFOutput, newContext) = fOutput(one, context)
                (new Computer(nextAddress, mem1, relativeBase, fInput, newFOutput, Continue), newContext)
            case Instruction(opCode@(JumpIfTrue | JumpIfFalse), Seq(m1, m2), Seq(o1, o2)) =>
                val (mem1, one) = readOperand(memory, o1, m1)
                val (mem2, two) = readOperand(mem1, o2, m2)
                val jump = opCode match {
                    case JumpIfFalse if (one == 0) => two.toInt
                    case JumpIfTrue if (one != 0) => two.toInt
                    case _ => nextAddress
                }
                (new Computer(jump, mem2, relativeBase, fInput, fOutput, Continue), context)
            case Instruction(RelativeOffset, Seq(m1), Seq(o1)) =>
                val (_, one) = readOperand(memory, o1, m1)
                val newRelativeBase = relativeBase + one.toInt
                (new Computer(nextAddress, memory, newRelativeBase, fInput, fOutput, Continue), context)
            case Instruction(Abort, Seq(), Seq()) =>
                (new Computer(nextAddress, memory, relativeBase, fInput, fOutput, Stop), context)

            case erroneousInstruction =>
                throw new RuntimeException(s"Erroneous instruction at address $instructionPointer: $erroneousInstruction")
        }
    }

    override def toString: String = {
        s"Computer\n(instructionPointer=$instructionPointer\n,memory=$memory\n,relativeBase=$relativeBase\n,inputCounter=$control\n)"
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
            case _ => throw new RuntimeException(s"Erroneous opcode at address $address: $opCode")
        }

        val operands = memory.slice(address + 1, address + 1 + numOperands)

        var i = 0
        var operandModes: List[OperandMode] = List()
        while (i < numOperands) {
            val last = modes % 10
            operandModes = operandModes ++ List(last)

            modes /= 10
            i += 1
        }

        Instruction(opCode, operandModes, operands)
    }

}

case class Instruction(opcode: OpCode, modes: Seq[OperandMode], operands: Seq[MemoryValue]) {
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

    def encode(): IndexedSeq[MemoryValue] = {
        var opValue: MemoryValue = opcode
        var baseMode = 100
        for (mode <- modes) {
            opValue += baseMode * mode
            baseMode *= 10
        }
        IndexedSeq(opValue) ++ operands.toIndexedSeq
    }
}
