package com.janboerman.aoc2019.day3

import scala.collection.mutable
import scala.io.Source

object Day3 extends App {

    val fileName = "src/main/resources/day3input.txt"
    val lines = Source.fromFile(fileName).getLines()
    val firstRoute = lines.next().split(",")
    val secondRoute = lines.next().split(",")

//    val firstRoute = Array("R75","D30","R83","U83","L12","D49","R71","U7","L72")
//    val secondRoute = Array("U62","R66","U55","R34","D71","R55","D58","R83")

    type Steps = Int

    val pointsOne = new mutable.HashMap[Point, Steps]()
    val pointsTwo = new mutable.HashMap[Point, Steps]()

    val origin: Point = Point(0, 0)
    pointsOne.put(origin, 0)
    pointsTwo.put(origin, 0)

    {
        var currentPoint = origin;
        for (instruction <- firstRoute) {
            currentPoint = readPoints(currentPoint, instruction, pointsOne)
        }
    }
    {
        var currentPoint = origin
        for (instruction <- secondRoute) {
            currentPoint = readPoints(currentPoint, instruction, pointsTwo)
        }
    }

    val intersections = pointsOne.keySet.intersect(pointsTwo.keySet).filter(_ != origin) //filter is bad because it's O(n), instead I should use Set#remove which is O(1) but I cba right now.

    val closestIntersection: Point = intersections.minBy(manhattenDistance(_, origin))
    val closestDistance = manhattenDistance(origin, closestIntersection)
    println(closestDistance)

    val shortestIntersection: Point = intersections.minBy(point => pointsOne(point) + pointsTwo(point))
    val sumSteps = pointsOne(shortestIntersection) + pointsTwo(shortestIntersection) //this calculation has already been done inside the minBy but I cba right now.
    println(sumSteps)

    def readPoints(from: Point, instruction: String, into: mutable.HashMap[Point, Steps]): Point = {
        val fx = from.x
        val fy = from.y
        val steps = into(from)

        instruction.toList match {
            case List('U', tail @ _*) =>
                val distance = tail.mkString.toInt
                val endY = fy + distance
                var step = steps
                for (y <- fy + 1 to endY) {
                    step += 1
                    into.getOrElseUpdate(Point(fx, y), step)
                }
                Point(fx, endY)
            case List('R', tail @ _*) =>
                val distance = tail.mkString.toInt
                val endX = fx + distance
                var step = steps
                for (x <- fx + 1 to endX) {
                    step += 1
                    into.getOrElseUpdate(Point(x, fy), step)
                }
                Point(endX, fy)
            case List('D', tail @ _*) =>
                val distance = tail.mkString.toInt
                val endY = fy - distance
                var step = steps
                for (y <- fy - 1 to(endY, -1)) {
                    step += 1
                    into.getOrElseUpdate(Point(fx, y), step)
                }
                Point(fx, endY)
            case List('L', tail @ _*) =>
                val distance = tail.mkString.toInt
                val endX = fx - distance
                var step = steps
                for (x <- fx -1 to(endX, -1)) {
                    step += 1
                    into.getOrElseUpdate(Point(x, fy), step)
                }
                Point(endX, fy)
        }
    }

    def manhattenDistance(one: Point, two: Point): Int = {
        Math.abs(two.x - one.x) + Math.abs(two.y - one.y)
    }

}

case class Point(x: Int, y: Int)
