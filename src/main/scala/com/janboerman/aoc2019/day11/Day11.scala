package com.janboerman.aoc2019.day11

import com.janboerman.aoc2019.util.{Direction, East, North, Point, South, West}

import scala.collection.mutable
import scala.io.Source

object Imports {
    type MemoryValue = Long
    type Address = Int
    type OpCode = Int
    type OperandMode = Int
    type Memory = IndexedSeq[MemoryValue]
    trait Input[CTX] extends ((CTX) => MemoryValue)
    trait Output[CTX] extends ((MemoryValue, CTX) => (Output[CTX], CTX))

    type Hull = IndexedSeq[IndexedSeq[Panel]]

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

    val Left = 0L
    val Right = 1L

    implicit class HullExtensions(val hull: Hull) extends AnyVal {
        @inline def apply(point: Point): Panel = apply(point.x, point.y)
        @inline def apply(x: Int, y: Int): Panel = hull(x)(y)
        @inline def set(point: Point, panel: Panel): Hull = set(point.x, point.y, panel)
        @inline def set(x: Int, y: Int, panel: Panel): Hull = hull.updated(y, hull(y).updated(x, panel))
    }
}

import Imports._


object Day11 extends App {
    val fileName = "src/main/resources/day11input.txt"
    val numbers: Memory = Source.fromFile(fileName).getLines().next().split(",").map(_.toLong).toIndexedSeq

    def newMemory(): Memory = {
        numbers
        Instruction.encode(Instruction(Output, Seq(ImmediateMode), Seq(1L))) ++
            Instruction.encode(Instruction(Output, Seq(ImmediateMode), Seq(Left))) ++
            Instruction.encode(Instruction(Output, Seq(ImmediateMode), Seq(1L))) ++
            Instruction.encode(Instruction(Output, Seq(ImmediateMode), Seq(Left))) ++
            Instruction.encode(Instruction(Output, Seq(ImmediateMode), Seq(1L))) ++
            Instruction.encode(Instruction(Output, Seq(ImmediateMode), Seq(Left))) ++
            Instruction.encode(Instruction(Output, Seq(ImmediateMode), Seq(1L))) ++
            Instruction.encode(Instruction(Output, Seq(ImmediateMode), Seq(Left))) ++
            Instruction.encode(Instruction(Abort, Seq(), Seq()))
    }

    val point = Point(2, 2)
    val emergencyHull: Hull = IndexedSeq.tabulate(5, 5)({case (x, y) => Panel(Black)})

    val paintedPoints = mutable.HashSet[Point]()
    var robot = Robot(point, North, emergencyHull, newMemory(), robot => paintedPoints.add(robot.position))

    while (robot.computer.control == Continue) {
        robot = robot.step(robot)
        robot.hull.foreach(println)
        println(robot)
    }

    println(paintedPoints.size)
}


sealed trait Colour
case object Black extends Colour
case object White extends Colour

case class Panel(colour: Colour) {
    override def toString(): String = colour match {
        case Black => "."
        case White => "#"
    }
}


object Robot {

    def apply(position: Point, direction: Direction, hull: Hull, memory: Memory, paintSideEffect: Robot => Unit): Robot = {
        val computer = Computer[Robot](0,memory, 0, fInput = inputFunction, fOutput = makePaintFunction(paintSideEffect), Continue)
        new Robot(position, direction, hull, computer)
    }

    val inputFunction: Input[Robot] = (robot) => robot.hull(robot.position) match {
        case Panel(Black) => 0L
        case Panel(White) => 1L
    }

    def makePaintFunction(sideEffect: Robot => Unit): Output[Robot] = (memoryValue, robot) => {
        val colour: Colour = memoryValue match {
            case 0L => Black
            case 1L => White
        }

        val newHull = robot.hull.set(robot.position, Panel(colour))
        val newRobot = new Robot(robot.position, robot.direction, newHull, robot.computer)

        sideEffect(newRobot)

        (makeTurnFunction(sideEffect), newRobot)
    }

    def makeTurnFunction(paintSideEffect: Robot => Unit): Output[Robot] = (memoryValue, robot) => {
        val newDirection: Direction = (memoryValue, robot.direction) match {
            case (Left, North) => West
            case (Right, North) => East
            case (Left, South) => East
            case (Right, South) => West
            case (Left, East) => North
            case (Right, East) => South
            case (Left, West) => South
            case (Right, West) => North
        }

        val newPosition = robot.position + newDirection.unitPoint
        val newRobot = new Robot(newPosition, newDirection, robot.hull, robot.computer)

        (makePaintFunction(paintSideEffect), newRobot)
    }

}

class Robot(val position: Point, val direction: Direction, val hull: Hull, val computer: Computer[Robot]) {
    override def toString: String = s"Robot($position, $direction)"

    def step(robot: Robot): Robot = {
        val (newComputer, updatedRobot) = robot.computer.step(robot)
        new Robot(updatedRobot.position, updatedRobot.direction, updatedRobot.hull, newComputer)
    }

    def currentColour: Colour = hull(position).colour
}






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
                    (newMem, newMem(address).intValue)
                case RelativeMode =>
                    val address = relativeBase + memoryValue.intValue
                    val newMem = resizeMemory(memory, address)
                    (newMem, newMem(address).intValue)
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

        def resizeMemory(memory: Memory, toAddress: Address): Memory = memory.padTo(toAddress + 1, 0L)
        def writeToMemory(memory: Memory, position: Address, value: MemoryValue): Memory = memory.updated(position, value)

        println()
        println(memory)
        println(instructionPointer)

        val instruction = Instruction.decode(memory, instructionPointer)

        println(instruction)
        println()

        val nextAddress = instructionPointer + instruction.operands.size + 1

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
                val inputValue = fInput(context)
                val newMemory = writeToMemory(mem1, position, inputValue)
                (new Computer(nextAddress, newMemory, relativeBase, fInput, fOutput, Continue), context)
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

    def encode(instruction: Instruction): IndexedSeq[MemoryValue] = {
        var opValue: MemoryValue = instruction.opcode
        var baseMode = 100
        for (mode <- instruction.modes) {
            opValue += baseMode * mode
            baseMode *= 10
        }
        IndexedSeq(opValue) ++ instruction.operands.toIndexedSeq
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
}

