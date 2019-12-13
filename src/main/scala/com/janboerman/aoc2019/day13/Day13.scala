package com.janboerman.aoc2019.day13

import com.janboerman.aoc2019.intcode.Types._
import com.janboerman.aoc2019.intcode.{Computer, Continue}
import com.janboerman.aoc2019.util.Point

import scala.io.{Source, StdIn}

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
        val computer: Computer[Part1.GridContext] = Computer(numbers, dummyInput, Part1.OutputX)
        val (cpu, ctx) = runComputer(computer, Part1.InitialGridContext)
        val result1 = ctx._3.values.count(_ == Block)
        println(result1)
    }

    {   //Part2
        val initialMemory = numbers.updated(0, BigInt(2))
        val computer: Computer[Part2.Context] = Computer(initialMemory, Part2.autoPilot, Part2.OutputX)
        val (cpu, ctx) = runComputer(computer, Part2.InitialContext)
        val result2 = ctx.score
        println(result2)
    }

    def runComputer[CTX](computer: Computer[CTX], context: CTX): (Computer[CTX], CTX) = {
        var cpu = computer
        var ctx = context
        while (cpu.control == Continue) {
            val (comp, contxt) = cpu.step(ctx)
            cpu = comp
            ctx = contxt
        }
        (cpu, ctx)
    }
}

case class GameState(joyStick: JoyStick,
                     x: PositionX,
                     y: PositionY,
                     grid: Grid,
                     score: Score) {

    def display(): Unit = {
        def showTile(tile: TileId): Char = tile match {
            case Empty => ' '
            case Wall => 'W'
            case Block => 'B'
            case HorizontalPaddle => '#'
            case Ball => 'O'
        }

        //can optimize this by having grid be a IndexedSeq[IndexedSeq[TileId]].
        val xs = grid.keys.map(_.x)
        val ys = grid.keys.map(_.y)
        val minX = if (xs.isEmpty) 0 else xs.min
        val minY = if (ys.isEmpty) 0 else ys.min
        val maxX = if (xs.isEmpty) 0 else xs.max
        val maxY = if (ys.isEmpty) 0 else ys.max

        for (y <- minY to maxY) {
            for (x <- minX to maxX) {
                grid.get(Point(x, y)) match {
                    case Some(tile) => print(showTile(tile))
                    case None => print(' ')
                }
            }
            println()
        }
        println(s"Score: $score")
    }
}

object Part2 {
    type Context = GameState
    val InitialContext = GameState(Neutral, 0, 0, EmptyGrid, 0)

    val manualJoystickInput: Input[Context] = {
        game =>
            game.display()

            println("Input Joystick: Left, Neutral or Right (L/N/R)")
            var joystickInput = -2
            while (joystickInput == -2) {
                val char = StdIn.readChar()
                joystickInput = char match {
                    case 'L' | 'l' => -1
                    case 'N' | 'n' => 0
                    case 'R' | 'r' => 1
                    case _ =>
                        println("invalid input, please choose from: {L, N, R}")
                        -2
                }
            }
            println()
            (manualJoystickInput, joystickInput)
    }

    val autoPilot: Input[Context] = {
        game =>
            //game.display()

            val maybeBall = game.grid.find({ case (_, tile) => tile == Ball })
            val maybePaddle = game.grid.find({ case (_, tile) => tile == HorizontalPaddle })

            val automaticJoystickInput: MemoryValue = (maybeBall, maybePaddle) match {
                case (Some((Point(ballX, _), _)), Some((Point(paddleX, _), _))) =>
                    if (ballX < paddleX) BigInt(-1)
                    else if (ballX > paddleX) BigInt(1)
                    else BigInt(0)
                case _ => BigInt(0)
            }

            //println()
            (autoPilot, automaticJoystickInput)
    }

    val OutputX: Output[Context] = {
        case (memValue, GameState(joyStick, x, y, grid, score)) =>
            val newX = memValue.intValue
            (OutputY, GameState(joyStick, newX, y, grid, score))
    }

    val OutputY: Output[Context] = {
        case (memValue, GameState(joyStick, x, y, grid, score)) =>
            val newY = memValue.intValue
            val newOutput = if (x == -1 && newY == 0) OutputScore else OutputTileId
            (newOutput, GameState(joyStick, x, newY, grid, score))
    }

    val OutputTileId: Output[Context] = {
        case (memValue, GameState(joyStick, x, y, grid, score)) =>
            val tileId = memValue.intValue
            val newGrid = if (tileId == Empty) {
                grid.removed(Point(x, y))
            } else {
                grid.updated(Point(x, y), tileId)
            }
            (OutputX, GameState(joyStick, x, y, newGrid, score))
    }

    val OutputScore: Output[Context] = {
        case (memValue, GameState(joyStick, x, y, grid, _)) =>
            val newScore = memValue.intValue
            (OutputX, GameState(joyStick, x, y, grid, newScore))
    }
}

object Part1 {
    type GridContext = (PositionX, PositionY, Grid)
    val InitialGridContext: GridContext = (0, 0, EmptyGrid)

    val OutputX: Output[GridContext] = {
        case (memValue, (_, y, grid)) =>
            val distanceX = memValue.intValue
            (OutputY, (distanceX, y, grid))
    }

    val OutputY: Output[GridContext] =  {
        case (memValue, (x, _, grid)) =>
            val distanceY = memValue.intValue
            (OutputTileId, (x, distanceY, grid))
    }

    val OutputTileId: Output[GridContext] = {
        case (memValue, (x, y, grid)) =>
            val tileId = memValue.intValue
            val newGrid = grid.updated(Point(x, y), tileId)
            (OutputX, (x, y, newGrid))
    }
}
