package com.janboerman.aoc2019.day12

import java.util.regex.Pattern

import scala.collection.immutable.IntMap
import scala.collection.mutable
import scala.io.Source

object Day12 extends App {
    val fileName = "src/main/resources/day12input.txt"
    val positions: IndexedSeq[Vec3d] = Source.fromFile(fileName).getLines().map(Vec3d.parse(_)).toIndexedSeq
    //val positions = IndexedSeq(Vec3d(-1, 0, 2), Vec3d(2, -10, -7), Vec3d(4, -8, 8), Vec3d(3, 5, -1))
    //val numSteps = 10

    val moons: IndexedSeq[Moon] = positions.map(p => Moon(p, Vec3d.zero))

    {   //part 1
        val numSteps = 1000
        var step = 0
        var state = moons
        while (step < numSteps) {
            state = Moon.simulate(moons)
            step += 1
        }

        val result1 = state.map(_.totalEnergy).sum
        println(result1)
    }
    {   //part 2
        val posXs = moons.map(_.position.x)
        val posYs = moons.map(_.position.y)
        val posZs = moons.map(_.position.z)

        val velXs = moons.map(_.velocity.x)
        val velYs = moons.map(_.velocity.y)
        val velZs = moons.map(_.velocity.z)

        var xs = posXs.zip(velXs)
        var ys = posYs.zip(velYs)
        var zs = posZs.zip(velZs)

        val setX = new mutable.HashSet[Seq[(Int, Int)]]()
        val setY = new mutable.HashSet[Seq[(Int, Int)]]()
        val setZ = new mutable.HashSet[Seq[(Int, Int)]]()

        var countX = 0
        var countY = 0
        var countZ = 0

        while (!setX.contains(xs)) {
            setX.add(xs)
            xs = simulateCompononent(xs)
            countX += 1
        }
        while (!setY.contains(ys)) {
            setY.add(ys)
            ys = simulateCompononent(ys)
            countY += 1
        }
        while (!setZ.contains(zs)) {
            setZ.add(zs)
            zs = simulateCompononent(zs)
            countZ += 1
        }

        //println(s"$countX, $countY, $countZ")

        import com.janboerman.aoc2019.util.Math.lcm
        val result2 = lcm(countX, lcm(countY, countZ))
        println(result2)
    }


    def simulateCompononent(components: IndexedSeq[(Int, Int)]): IndexedSeq[(Int, Int)] = {
        val gravityMap = new Array[Int](components.size)

        for (i <- 0 until components.size) {
            for (j <- (i+1) until components.size) {
                val (p1, v1) = components(i)
                val (p2, v2) = components(j)
                val gravity = if (p1 < p2) 1 else if (p1 > p2) -1 else 0

                gravityMap(i) += gravity
                gravityMap(j) -= gravity
            }
        }

        val result = new Array[(Int, Int)](components.size)
        for (i <- 0 until components.size) {
            result(i) = components(i)
        }

        for (idx <- 0 until gravityMap.length) {
            val (oldPos, oldVel) = result(idx)
            val newVel = oldVel + gravityMap(idx)
            val newPos = oldPos + newVel
            result(idx) = (newPos, newVel)
        }

        result.toIndexedSeq
    }
}

object Moon {

    def simulate(moons: IndexedSeq[Moon]): IndexedSeq[Moon] = {
        val gravities = for {
            i <- 0 until moons.size;
            j <- 0 until moons.size if (j != i);
            (moon1, moon2) = (moons(i), moons(j))
            (gravity1, gravity2) = calculateGravities(moon1 , moon2)
        } yield (i, j, gravity1, gravity2)

        val totalGravities = gravities.foldLeft[Map[Int, Vec3d]](IntMap[Vec3d]().withDefaultValue(Vec3d.zero))({
            case (intMap, (idx1, idx2, g1,g2)) =>
                intMap.updatedWith(idx1)({ case Some(g) => Some(g + g1); case None => Some(g1)})
                    .updatedWith(idx2)({ case Some(g) => Some(g + g2); case None => Some(g2)})
        }).map({ case (idx, g) => (idx, g / 2)})

        var result = moons
        for ((idx, g) <- totalGravities) {
            result = result.updated(idx, result(idx).applyGravity(g))
        }
        result = result.map(_.applyVelocity())
        result
    }

    def calculateGravities(one: Moon, two: Moon): (Vec3d, Vec3d) = {
        def compare(one: Int, two: Int) = {
            if (one == two) {
                (0, 0)
            } else if (one < two) {
                (1, -1)
            } else {
                (-1, 1)
            }
        }

        val (oneX, twoX) = compare(one.position.x, two.position.x)
        val (oneY, twoY) = compare(one.position.y, two.position.y)
        val (oneZ, twoZ) = compare(one.position.z, two.position.z)

        (Vec3d(oneX, oneY, oneZ), Vec3d(twoX, twoY, twoZ))
    }
}

case class Moon(position: Vec3d, velocity: Vec3d) {

    def applyGravity(gravity: Vec3d): Moon = Moon(position, velocity + gravity)
    def applyVelocity(): Moon = Moon(position + velocity, velocity)

    def potentialEnergy: Int = Math.abs(position.x) + Math.abs(position.y) + Math.abs(position.z)
    def kineticEnergy: Int = Math.abs(velocity.x) + Math.abs(velocity.y) + Math.abs(velocity.z)
    def totalEnergy: Int = potentialEnergy * kineticEnergy

}


object Vec3d {
    /*  <x=8, y=0, z=8>
        <x=0, y=-5, z=-10>
        <x=16, y=10, z=-5>
        <x=19, y=-10, z=-7>*/
    val zero = withAll(0)

    def withAll(xyz: Int) = Vec3d(xyz, xyz, xyz)

    private val integerPattern = Pattern.compile("-?\\d+")

    def parse(string: String): Vec3d = {
        val matcher = integerPattern.matcher(string)
        val x = {matcher.find(); matcher.group().toInt}
        val y = {matcher.find(); matcher.group().toInt}
        val z = {matcher.find(); matcher.group().toInt}
        Vec3d(x, y, z)
    }

}

case class Vec3d(x: Int, y: Int, z: Int) {
    def +(that: Vec3d): Vec3d = Vec3d(x + that.x, y + that.y, z + that.z)
    def -(that: Vec3d): Vec3d = Vec3d(x - that.x, y - that.y, z - that.z)
    def *(that: Vec3d): Vec3d = Vec3d(x * that.x, y * that.y, z * that.z)
    def /(that: Vec3d): Vec3d = Vec3d(x / that.x, y / that.y, z / that.z)

    def *(scalar: Int): Vec3d = Vec3d(x * scalar, y * scalar, z * scalar)
    def /(scalar: Int): Vec3d = Vec3d(x / scalar, y / scalar, z / scalar)
}
