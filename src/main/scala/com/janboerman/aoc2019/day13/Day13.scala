package com.janboerman.aoc2019.day13

import com.janboerman.aoc2019.intcode.Types._
import com.janboerman.aoc2019.intcode.{Computer, Continue}
import com.janboerman.aoc2019.util.Point

import scala.io.Source

object Types {
    type Grid = Map[Point, TileId]
    type TileId = Int
    type PositionX = Int
    type PositionY = Int
    type JoyStick = Int
    type Score = Int

    val EmptyGrid: Grid = Map.empty[Point, TileId]

    val Empty = 0
    val Wall = 1
    val Block = 2
    val HorizontalPaddle = 3
    val Ball = 4

    val Neutral = 0
    val TiltedLeft = -1
    val TiltedRight = 1
}
import Types._

object Day13 extends App {

    val fileName = "src/main/resources/day13input.txt"
    val numbers: Memory = Source.fromFile(fileName).getLines().next().split(",").map(string => new BigInt(new java.math.BigInteger(string))).toIndexedSeq

    {   //Part1
        var cpu: Computer[Part1.GridContext] = Computer(numbers, dummyInput, Part1.OutputX)
        var context = Part1.InitialGridContext
        while (cpu.control == Continue) {
            val (c, ctx) = cpu.step(context)
            cpu = c
            context = ctx
        }
        val result1 = context._3.values.count(_ == Block)
        println(result1)
    }

    {   //Part2
        //TODO simulate the game - (just like the robot)
        //TODO be sure to update the grid as blocks get broken

        val initialMemory = numbers.updated(0, BigInt(2))
        var context = Part2.InitialContext
        var cpu: Computer[Part2.Context] = Computer(initialMemory, Part2.inputJoyStick, Part2.OutputX)
        while (cpu.control == Continue) {
            val (c, ctx) = cpu.step(context)
            cpu = c
            context = ctx
        }
        val result2 = context._5
        println(result2)
    }
}


object Part2 {
    type Context = (JoyStick, PositionX, PositionY, Grid, Score)
    val InitialContext = (Neutral, 0, 0, EmptyGrid, 0)

    val inputJoyStick: Input[Context] = {
        case (joyStick, x, y, grid, score) =>
            (joyStick, inputJoyStick)
    }

    val OutputX: Output[Context] = {
        case (memValue, (joyStick, _, y, grid, score)) =>
            val newX = memValue.intValue
            (OutputY, (joyStick, newX, y, grid, score))
    }

    val OutputY: Output[Context] = {
        case (memValue, (joyStick, x, _, grid, score)) =>
            val newY = memValue.intValue
            val newOutput = if (x == -1 && newY == 0) OutputScore else OutputTileId
            (newOutput, (joyStick, x, newY, grid, score))
    }

    val OutputTileId: Output[Context] = {
        case (memValue, (joyStick, x, y, grid, score)) =>
            val tileId = memValue.intValue
            val newGrid = grid.updated(Point(x, y), tileId)
            (OutputX, (joyStick, x, y, newGrid, score))
    }

    val OutputScore: Output[Context] = {
        case (memValue, (joyStick, x, y, grid, score)) =>
            val newCore = memValue.intValue
            (OutputX, (joyStick, x, y, grid, newCore))
    }
}

object Part1 {
    type GridContext = (PositionX, PositionY, Grid)
    val InitialGridContext: GridContext = (0, 0, EmptyGrid)

    val OutputX: Output[GridContext] = {
        case (memValue, (_, y, grid)) => {
            val distanceX = memValue.intValue
            (OutputY, (distanceX, y, grid))
        }
    }

    val OutputY: Output[GridContext] =  {
        case (memValue, (x, _, grid)) => {
            val distanceY = memValue.intValue
            (OutputTileId, (x, distanceY, grid))
        }
    }

    val OutputTileId: Output[GridContext] = {
        case (memValue, (x, y, grid)) => {
            val tileId = memValue.intValue
            val newGrid = grid.updated(Point(x, y), tileId)
            (OutputX, (x, y, newGrid))
        }
    }
}
