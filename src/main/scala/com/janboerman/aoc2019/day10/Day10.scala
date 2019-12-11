package com.janboerman.aoc2019.day10

import scala.io.Source

object Imports {
    def gcd(a: Int, b: Int): Int = {
        if (b == 0) a else gcd(b, a % b)
    }

    def angle(centre: Point, point: Point): Double = {
        if (centre == point) return Math.PI * 2

        val diff = point - centre
//        println()
//        println(s"angle (diff.x, diff.y) = (${diff.x}, ${diff.y})")

        if (diff.x == 0 && diff.y < 0) return 0D
        if (diff.x > 0 && diff.y == 0) return Math.PI * 0.5
        if (diff.x == 0 && diff.y > 0) return Math.PI
        if (diff.x < 0 && diff.y == 0) return Math.PI * 1.5

        def inverseTan(opposite: Int, adjacent: Int): Double = Math.atan(opposite.toDouble / adjacent.toDouble)

        if (diff.x == 0 && diff.y < 0) 0
        else if (diff.x > 0 && diff.y < 0 && diff.x < -diff.y) inverseTan(diff.x, -diff.y)
        else if (diff.x > 0 && diff.y < 0 && diff.x == -diff.y) Math.PI * 0.25
        else if (diff.x > 0 && diff.y < 0 && diff.x > -diff.y) Math.PI * 0.5 - inverseTan(-diff.y,  diff.x)
        else if (diff.x > 0 && diff.y == 0) Math.PI * 0.5
        else if (diff.x > 0 && diff.y > 0 && diff.x > diff.y) Math.PI * 0.5 + inverseTan(diff.y, diff.x)
        else if (diff.x > 0 && diff.y > 0 && diff.x == diff.y) Math.PI * 0.75
        else if (diff.x > 0 && diff.y > 0 && diff.x < diff.y) Math.PI - inverseTan(diff.x, diff.y)
        else if (diff.x == 0 && diff.y > 0) Math.PI
        else if (diff.x < 0 && diff.y > 0 && -diff.x < diff.y) Math.PI + inverseTan(-diff.x, diff.y)
        else if (diff.x < 0 && diff.y > 0 && -diff.x == diff.y) Math.PI * 1.25
        else if (diff.x < 0 && diff.y > 0 && -diff.x > diff.y) Math.PI * 1.5 - inverseTan(diff.y, -diff.x)
        else if (diff.x < 0 && diff.y == 0) Math.PI * 1.5
        else if (diff.x < 0 && diff.y < 0 && -diff.x > -diff.y) Math.PI * 1.5 + inverseTan(-diff.y, -diff.x)
        else if (diff.x < 0 && diff.y < 0 && -diff.x == -diff.y) Math.PI * 1.75
        else if (diff.x < 0 && diff.y < 0 && -diff.x < -diff.y) Math.PI * 2 - inverseTan(-diff.x, -diff.y)
        else throw new RuntimeException(s"Unmatched case ($diff.x, $diff.y")
    }
}
import Imports._

sealed trait MapObject
case object Asteroid extends MapObject {
    override def toString(): String = "###"
}
case object Empty extends MapObject {
    override def toString(): String = "..."
}
case object Station extends MapObject {
    override def toString(): String = "XXX"
}
case class Vaporised(count: Int) extends MapObject {
    override def toString(): String = if (count < 10) s"  $count"
        else if (count < 100) s" $count"
        else s"$count"
}

object Day10 extends App {

    type AsteroidMap = IndexedSeq[IndexedSeq[MapObject]]

    val fileName = "src/main/resources/day10input.txt"
    val map: AsteroidMap = makeMap(Source.fromFile(fileName).getLines())

    import scala.jdk.CollectionConverters._
    def makeMap(lines: String): AsteroidMap = makeMap(lines.lines().iterator().asScala)
    def makeMap(lines: Iterator[String]): AsteroidMap = lines.map(_ map {
        case '.' => Empty
        case '#' => Asteroid
        case 'X' => Station
    }).toIndexedSeq

//    val example = makeMap(".#..#\n.....\n#####\n....#\n...##")
//    example.foreach(println)
//    println(solutionOne(example))

    val (resultX, resultY, resultCount) = solutionOne(map)
    println(resultCount)

//    val example21 = makeMap(".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##")
//    example21.foreach(println)
//
//    val example21centre = Point(8, 3)
//    val (point21, count21, newMap21) = solutionTwo(example21, example21centre, 9)
//
//    println()
//    newMap21.foreach(println)
//    println(point21)

//    var test17 = makeMap(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
//    val test17Center = Point(11, 13)
//    test17 = test17.updated(test17Center.y, test17(test17Center.y).updated(test17Center.x, Station))
//
//    val (point17, count17, newMap17) = solutionTwo(test17, test17Center, 200)
//    println()
//    newMap17.foreach(println)
//    println(point17)

    val mapWithCentre = map.updated(resultY, map(resultY).updated(resultX, Station))
    val (Point(x, y), count, newMap) = solutionTwo(mapWithCentre, Point(resultX, resultY), 200)
    println(100 * x + y)


    def solutionTwo(map: AsteroidMap, centre: Point, maxCount: Int): (Point, Int, AsteroidMap) = {
        val height = map.size
        val width = map(0).size

        implicit val centerPoint: Point = centre
        val coordinates: List[Point] = (0 until width).flatMap(x => (0 until height).map(y => Point(x, y)))
            .filter(hasAsteroid(map, _))
            .filterNot(_ == centre)
            .toList
            .sorted

        var infiniteList: LazyList[Point] = LazyList.concat(LazyList.continually(coordinates): _*)

        var point = infiniteList.head
        var count = 0
        var m = map

        while (count < maxCount) {
            val Point(dirX, dirY) = point - centre

            val done = step(m, centre.x, centre.y, dirX, dirY)(count+1) match {
                case Some((foundPoint, newMap)) =>
                    count += 1
                    m = newMap
                    point = foundPoint
                    count == maxCount
                case None => false
            }

            if (!done) {
                val (newPoint, newList) = nextPoint(point, infiniteList, centre)
                point = newPoint
                infiniteList = newList
            }
        }

        (point, count, m)
    }

    def nextPoint(lastPoint: Point, supplier: LazyList[Point], centre: Point): (Point, LazyList[Point]) = {
        val difference = lastPoint - centre

        var newPoint = supplier.head
        var newSupplier = supplier.tail
        var newDifference = newPoint - centre

        while (newDifference.isMutipleOf(difference)) {
            newPoint = newSupplier.head
            newSupplier = newSupplier.tail
            newDifference = newPoint - centre
        }

        (newPoint, newSupplier)
    }



    //only called with x's and y's not on the diagonal, so, assertNot(directionX == directionY && directionX == -directionY)
    def step(map: AsteroidMap, posX: Int, posY: Int, directionX: Int, directionY: Int)(count: Int): Option[(Point, AsteroidMap)] = {
        val height = map.size
        val width = map(0).size

        val divisor = Math.abs(gcd(directionX, directionY))
        val stepX = if (divisor > 1) directionX / divisor else directionX
        val stepY = if (divisor > 1) directionY / divisor else directionY

        var y = posY + stepY
        var x = posX + stepX

        while (0 <= x && x < width && 0 <= y && y < height) {
            if (hasAsteroid(map, x, y)) {
                return Some(Point(x, y), map.updated(y, map(y).updated(x, Vaporised(count))))
            }

            x += stepX
            y += stepY
        }

        None
    }

    def solutionOne(map: AsteroidMap): (Int, Int, Int) = {
        val height = map.size
        val width = map(0).size

        //try all starting positions
        val (x, y, count) = (0 until width).flatMap(x => (0 until height).map(y => (x, y)))
            .filter({ case (x, y) => hasAsteroid(map, x, y)})
            .map({ case (x, y) => (x, y, countAsteroids(map, x, y))})
            .maxBy(_._3)

        (x, y, count)
    }

    def hasAsteroid(map: AsteroidMap, x: Int, y: Int): Boolean = map(y)(x) == Asteroid
    def hasAsteroid(map: AsteroidMap, point: Point): Boolean = hasAsteroid(map, point.x, point.y)

    def countAsteroids(map: AsteroidMap, posX: Int, posY: Int): Int = {
        val height = map.size
        val width = map(0).size

        var count = 0
        val largestStepY = Math.max(posY, height - posY)
        val largestStepX = Math.max(posX, width - posX)

        //horizontal, vertical
        if (((posX + 1) until width).exists(hasAsteroid(map, _, posY))) count += 1
        if (((posX - 1) until 0 by -1).exists(hasAsteroid(map, _, posY))) count += 1
        if (((posY + 1) until height).exists(hasAsteroid(map, posX, _))) count += 1
        if (((posY - 1) until 0 by -1).exists(hasAsteroid(map, posX, _))) count += 1

        //find diagonal asteroids
        val xSteps = List.from(1 to largestStepX)
        val ySteps = List.from(1 to largestStepY)
        for (stepX <- xSteps ++ xSteps.map(_ * -1)) {
            for (stepY <- ySteps ++ ySteps.map(_ * -1)) {
                if (!(stepX == 0 && stepY == 0)) {
                    if (Math.abs(gcd(stepX, stepY)) == 1) {
                        if (hasAsteroidFor(map, posX, posY, stepX, stepY, width, height)) {
                            count += 1
                        }
                    }
                }
            }
        }

        count
    }

    def hasAsteroidFor(map: AsteroidMap, posX: Int, posY: Int, stepX: Int, stepY: Int, width: Int, height: Int): Boolean = {
        val xRange: Iterable[Int] = stepX match {
            case _ if stepX > 0 => (posX + stepX) until width by stepX
            case _ if stepX < 0 => (posX + stepX) to 0 by stepX
            case _ => LazyList.continually(posX)
        }
        val yRange: Iterable[Int] = stepY match {
            case _ if stepY > 0 => (posY + stepY) until height by stepY
            case _ if stepY < 0 => (posY + stepY) to 0 by stepY
            case _ => LazyList.continually(posY)
        }

        xRange.zip(yRange).exists({case (x, y) => hasAsteroid(map, x, y)})
    }

}

object Point {
    private lazy val Zero = Point(0, 0)

    implicit def ordering(implicit centerPoint: Point): Ordering[Point] = new Ordering[Point] {
        override def compare(one: Point, two: Point): Int = {
            val angleOne = angle(centerPoint, one)
            val angleTwo = angle(centerPoint, two)

            val angleResult = angleOne.compareTo(angleTwo)
            if (angleResult != 0)
                angleResult
            else
                Math.abs(centerPoint.manhattanDistance(one)).compareTo(Math.abs(centerPoint.manhattanDistance(two)))
        }
    }

    def zero: Point = Zero
}

case class Point(x: Int, y: Int) {

    def manhattanDistance(that: Point): Int = Math.abs(that.x - this.x) + Math.abs(that.y - this.y)

    def add(that: Point): Point = Point(this.x + that.x, this.y + that.y)
    def subtract(that: Point): Point = Point(this.x - that.x, this.y - that.y)

    def -(that: Point): Point = subtract(that)
    def +(that: Point): Point = add(that)

    def isMutipleOf(other: Point): Boolean = {
        this.minimise == other.minimise
    }

    def minimise: Point = {
        if (x == 0) {
            if (y > 0) Point(0, 1)
            else if (y < 0) Point(0, -1)
            else Point(0, 0)
        } else if (y == 0) {
            if (x > 0) Point (1, 0)
            else if (x < 0) Point(-1, 0)
            else Point(0, 0)
        } else {
            val divisor = Math.abs(gcd(x, y))
            Point(x / divisor, y / divisor)
        }
    }
}