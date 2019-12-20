package com.janboerman.aoc2019.day15

import com.janboerman.aoc2019.intcode.{Computer, Continue}
import com.janboerman.aoc2019.intcode.Types.{Input, Memory, MemoryValue, Output}
import com.janboerman.aoc2019.util.Point

import scala.io.Source

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

    val newDroid = Droid(Point(0, 0), Map.empty, West)

    var computer = Computer[Droid](numbers, Part1.input, Part1.output)
    var droid = newDroid
    while (computer.control == Continue && !droid.grid.get(droid.position).contains(Oxygen)) {
        println("loop")
        droid.display()

        val (cpu, d) = computer.step(droid)
        computer = cpu
        droid = d
    }
    println(droid.position)

}

object Part1 {
    val input: Input[Droid] = {
        case Surroundings(Wall, Wall, Wall, _) =>
            (input, West)
        case Surroundings(Wall, Wall, _, _) =>
            (input, South)
        case Surroundings(Wall, _, _, _) =>
            (input, East)
        case droid =>
            (input, North)
    }

    val output: Output[Droid] = {
        (reply, droid) =>
            val newDroid = droid.update(reply)
            (output, newDroid)
    }
}

object Surroundings {
    def unapply(droid: Droid): Option[(Tile, Tile, Tile, Tile)] = {
        //north, east, south, west
        val northPoint = droid.position + Point(0, -1)
        val southPoint = droid.position + Point(0, 1)
        val westPoint = droid.position + Point(-1, 0)
        val eastPoint = droid.position + Point(1, 0)

        val tileNorth = droid.grid.getOrElse(northPoint, Unexplored)
        val tileSouth = droid.grid.getOrElse(southPoint, Unexplored)
        val tileWest = droid.grid.getOrElse(westPoint, Unexplored)
        val tileEast = droid.grid.getOrElse(eastPoint, Unexplored)

        Some((tileNorth, tileEast, tileSouth, tileWest))
    }
}

case class Droid(position: Point, grid: Grid, lastMoveAttempt: Command) {
    def update(statusCode: Reply): Droid = {
        val updatePosition = lastMoveAttempt match {
            case North => position + Point(0, -1)
            case South => position + Point(0, 1)
            case West => position + Point(-1, 0)
            case East => position + Point(1, 0)
        }

        val (newPos, newGrid) = statusCode match {
            case WallHit => (position, grid.updated(updatePosition, Wall))
            case Moved => (updatePosition, grid.updated(position, Empty).updated(updatePosition, Empty))
            case MovedOxygenSystem => (updatePosition, grid.updated(position, Empty).updated(updatePosition, Oxygen))
            case invalidStatus =>
                println(s"erroneous status code: $invalidStatus")
                (position, grid)
        }

        Droid(newPos, newGrid, lastMoveAttempt)
    }


    def display(): Unit = {
        def showTile(tile: Tile): Char = tile match {
            case Unexplored => '~'
            case Empty => '.'
            case Wall => '#'
            case Oxygen => 'O'
        }

        val xs = grid.keys.map(_.x)
        val ys = grid.keys.map(_.y)
        val minX = if (xs.isEmpty) 0 else xs.min
        val minY = if (ys.isEmpty) 0 else ys.min
        val maxX = if (xs.isEmpty) 0 else xs.max
        val maxY = if (ys.isEmpty) 0 else ys.max

        for (y <- minY to maxY) {
            for (x <- minX to maxX) {
                position match {
                    case _ =>
                        grid.get(Point(x, y)) match {
                            case Some(tile) => print(showTile(tile))
                            case None => print('~')
                    }
                }
            }
            println()
        }
        println(s"Position: $position")
        println()
    }
}
