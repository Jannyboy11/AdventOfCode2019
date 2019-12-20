package com.janboerman.aoc2019.day15

import com.janboerman.aoc2019.intcode.{Computer, Continue}
import com.janboerman.aoc2019.intcode.Types.{Input, Memory, MemoryValue, Output}
import com.janboerman.aoc2019.util.Point

import scala.io.{Source, StdIn}

object Types {
    type Tile = Int
    type Command = MemoryValue
    type Reply = MemoryValue
    type Grid = Map[Point, Tile]

    val North = BigInt(1)
    val South = BigInt(2)
    val West = BigInt(3)
    val East = BigInt(4)

    val WallHit = BigInt(0)
    val Moved = BigInt(1)
    val MovedOxygenSystem = BigInt(2)

    val Empty = 0
    val Wall = 1
    val Oxygen = 2
    val Unexplored = 3
}
import Types._

object Day15 extends App {

    val fileName = "src/main/resources/day15input.txt"
    val numbers: Memory = Source.fromFile(fileName).getLines().next().split(",").map(string => new BigInt(new java.math.BigInteger(string))).toIndexedSeq

    val newDroid = Droid(Point(0, 0), Map.empty, North)

    var computer = Computer[Droid](numbers, Part1.autoPilot, Part1.output)
    var droid = newDroid
    while (computer.control == Continue && !droid.grid.get(droid.position).contains(Oxygen)) {
        val (cpu, d) = computer.step(droid)
        computer = cpu
        droid = d
    }
    val result1 = droid.movesMap(droid.position)
    println(result1)



}
import Droid._

object Part1 {
    val autoPilot: Input[Droid] = {
        case droid@Droid(position, grid, lastMove, movesMap) =>
            droid.display()
            var attemptDirections = lastMove match {
                case North => List(East, North, West, South)
                case West => List(North, West, South, East)
                case South => List(West, South, East, North)
                case East => List(South, East, North, West)
            }

            var directionPoint = nextPoint(position, attemptDirections.head)
            while (grid.getOrElse(directionPoint, Unexplored) == Wall) {
                attemptDirections = attemptDirections.tail
                directionPoint = nextPoint(position, attemptDirections.head)
            }

            val direction = attemptDirections.head
            val newGrid = if (attemptDirections.tail.isEmpty) grid.updated(position, Wall) else grid
            val newDroid = Droid(position, newGrid, direction, movesMap)

            (autoPilot, newDroid, direction)
    }


    val manualInput: Input[Droid] = {
        case droid@Droid(position, grid, lastMove, movesMap) =>
            droid.display()
            println("Enter direction [N]orth, [S]outh, [E]ast, [W]est")

            var direction = BigInt(-1)
            while (direction == BigInt(-1)) {
                val char = StdIn.readChar()
                direction = char match {
                    case 'N' | 'n' => North
                    case 'S' | 's' => South
                    case 'E' | 'e' => East
                    case 'W' | 'w' => West
                    case _ => BigInt(-1)
                }
                if (direction == BigInt(-1)) println("Invalid direction, please enter N, S, E or W")
            }

            val newDroid = Droid(position, grid, direction, movesMap)
            (manualInput, newDroid, direction)
    }

    val output: Output[Droid] = {
        (reply, droid) =>
            val newDroid = droid.update(reply)
            (output, newDroid)
    }
}

object Droid {
    def nextPoint(point: Point, direction: Command): Point = direction match {
        case North => point + Point(0, -1)
        case South => point + Point(0, 1)
        case West => point + Point(-1, 0)
        case East => point + Point(1, 0)
    }

    def apply(position: Point, grid: Grid, lastMoveAttempt: Command): Droid = {
        new Droid(position, grid, lastMoveAttempt, Map(position -> 0))
    }
}

case class Droid(position: Point, grid: Grid, lastMoveAttempt: Command, movesMap: Map[Point, Int]) {
    def update(statusCode: Reply): Droid = {
        val updatePosition = nextPoint(position, lastMoveAttempt)

        val (newPos, newGrid) = statusCode match {
            case WallHit => (position, grid.updated(updatePosition, Wall))
            case Moved => (updatePosition, grid.updated(position, Empty).updated(updatePosition, Empty))
            case MovedOxygenSystem => (updatePosition, grid.updated(position, Empty).updated(updatePosition, Oxygen))
            case invalidStatus =>
                println(s"erroneous status code: $invalidStatus")
                (position, grid)
        }

        val currentPlusOne = movesMap(position) + 1
        val newMovesMap = movesMap.get(updatePosition) match {
            case Some(moveCount) if moveCount <= currentPlusOne => movesMap
            case _ => movesMap.updated(updatePosition, currentPlusOne)
        }
        Droid(newPos, newGrid, lastMoveAttempt, newMovesMap)
    }


    def display(): Unit = {
        def showTile(tile: Tile): Char = tile match {
            case Empty => '.'
            case Wall => '\u2588'
            case Oxygen => 'O'
        }

        val xs = Some(position.x) ++ grid.keys.map(_.x)
        val ys = Some(position.y) ++ grid.keys.map(_.y)
        val minX = (if (xs.isEmpty) 0 else xs.min) - 1
        val minY = (if (ys.isEmpty) 0 else ys.min) - 1
        val maxX = (if (xs.isEmpty) 0 else xs.max) + 1
        val maxY = (if (ys.isEmpty) 0 else ys.max) + 1

        for (y <- minY to maxY) {
            for (x <- minX to maxX) {
                if (x == position.x && y == position.y) {
                    print('D')
                } else {
                    grid.get(Point(x, y)) match {
                        case Some(tile) => print(showTile(tile))
                        case None => print('\u2591')
                    }
                }
            }
            println()
        }
        println(s"Position: $position")
        println()
    }
}
