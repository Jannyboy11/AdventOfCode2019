package com.janboerman.aoc2019.day11

import com.janboerman.aoc2019.intcode.Types._
import com.janboerman.aoc2019.intcode._
import com.janboerman.aoc2019.util.{Direction, East, North, Point, South, West}

import scala.collection.mutable
import scala.io.Source

object Imports {
    type Colour = MemoryValue
    type Hull = Map[Point, Panel]

    val Left = BigInt(0)
    val Right = BigInt(1)

    val Black = BigInt(0)
    val White = BigInt(1)
}

import Imports._


object Day11 extends App {
    val fileName = "src/main/resources/day11input.txt"
    val numbers: Memory = Source.fromFile(fileName).getLines().next().split(",").map(string => new BigInt(new java.math.BigInteger(string))).toIndexedSeq

    def makeMemory(): Memory = {
//        import com.janboerman.aoc2019.intcode.Opcodes._
//        import com.janboerman.aoc2019.intcode.OperandModes._
//        Instruction(Output, Seq(ImmediateMode), Seq(White)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Left)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Black)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Left)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(White)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Left)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(White)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Left)).encode() ++
//            Instruction(Input, Seq(PositionMode), Seq(0)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Black)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Right)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(White)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Left)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(White)).encode() ++
//            Instruction(Output, Seq(ImmediateMode), Seq(Left)).encode() ++
//            Instruction(Abort, Seq(), Seq()).encode()

        numbers
    }

    val emergencyHull: Hull = Map[Point, Panel]().withDefaultValue(Panel(Black))

    {   //part1
        val paintedPoints = mutable.HashSet[Point]()
        val startingPoint = Point(0, 0)
        var robot = Robot(startingPoint, North, emergencyHull, makeMemory(), robot => paintedPoints.add(robot.position))

        while (robot.computer.control == Continue) {
            //        println("Hull = " + robot.hull)
            //        println(robot)
            //        println()
            robot = robot.step(robot)
        }

        println(paintedPoints.size)
    }
    {   //part2
        val startingPoint = Point(0, 0)
        val hullWithWhitePanel = emergencyHull.updated(startingPoint, Panel(White))
        var robot = Robot(startingPoint, North, hullWithWhitePanel, makeMemory(), robot => ())

        while (robot.computer.control == Continue) {
            robot = robot.step(robot)
        }

        val lowestY = robot.hull.keys.minBy(_.y).y
        val highestY = robot.hull.keys.maxBy(_.y).y
        val lowestX = robot.hull.keys.minBy(_.x).x
        val highestX = robot.hull.keys.maxBy(_.x).x

        for (y <- lowestY to highestY) {
            for (x <- lowestX to highestX) {
                print(robot.hull(Point(x, y)))
            }
            println()
        }
    }
}


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
        case Panel(inputColour) =>
            //println(s"SUPPLYING INPUT ${inputColour}")
            (inputFunction, inputColour)
    }

    def makePaintFunction(sideEffect: Robot => Unit): Output[Robot] = (memoryValue, robot) => {
        val colour: Colour = memoryValue
        //println(s"PAINTING ${if (colour == Black) "BLACK" else "WHITE"}")

        val newHull = robot.hull.updated(robot.position, Panel(colour))
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

        val addPoint = newDirection match {
            case North => Point(0, -1)
            case South => Point(0, 1)
            case West => Point(-1, 0)
            case East => Point(1, 0)
        }

        val newPosition = robot.position + addPoint
        val newRobot = new Robot(newPosition, newDirection, robot.hull, robot.computer)

        //println(s"TURNING ${if (memoryValue == Left) "LEFT" else "RIGHT"}")

        (makePaintFunction(paintSideEffect), newRobot)
    }

}

class Robot(val position: Point, val direction: Direction, val hull: Hull, val computer: Computer[Robot]) {
    override def toString: String = s"Robot($position, ${direction.getClass.getSimpleName})"

    def step(robot: Robot): Robot = {
        val (newComputer, updatedRobot) = robot.computer.step(robot)
        new Robot(updatedRobot.position, updatedRobot.direction, updatedRobot.hull, newComputer)
    }

    def currentColour: Colour = hull(position).colour
}


