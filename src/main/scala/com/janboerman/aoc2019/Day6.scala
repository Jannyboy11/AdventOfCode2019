package com.janboerman.aoc2019

import com.janboerman.aoc2019.Aliases._

import scala.collection.mutable
import scala.io.Source

object Aliases {
    type ID = String
}

object Day6 extends App {

    val fileName = "src/main/resources/day6input.txt"
    val lines = Source.fromFile(fileName).getLines()
    //val lines = List("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")
    val orbits = lines.map(Orbit.decode)

    //val orbitMap: mutable.Map[ID, mutable.ListBuffer[ID]] = new mutable.HashMap();
    val spinningAround: mutable.Map[ID, ID] = new mutable.HashMap()
    for (orbit <- orbits) {
        //orbitMap.getOrElseUpdate(orbit.centerPoint, new mutable.ListBuffer()).addOne(orbit.spinning)
        spinningAround.put(orbit.spinning, orbit.centerPoint)
    }

    val countMap: mutable.Map[ID, Int] = new mutable.HashMap()

    spinningAround.keySet.foreach(id => countOrbits(id, spinningAround, countMap))
    val result1 = countMap.values.sum
    println(result1)

    val chain1 = chain("YOU")
    val chain2 = chain("SAN")
    val commonLength = chain1.zip(chain2).takeWhile {case (x, y) =>  x == y}.size
    val result2 = (chain1.size - commonLength) + (chain2.size - commonLength) - 2
    println(result2)

    def countOrbits(id: ID, spinningMap: mutable.Map[ID, ID], countMap: mutable.Map[ID, Int]): Int = {
        if ("COM".equals(id)) return 0
        countMap.get(id) match {
            case Some(count) => count
            case None =>
                val distance = 1 + countOrbits(spinningMap(id), spinningMap, countMap)
                countMap.put(id, distance)
                distance
        }
    }

    def chain(id: ID): mutable.ListBuffer[ID] = {
        if ("COM".equals(id)) return mutable.ListBuffer("COM")

        chain(spinningAround(id)).addOne(id)
    }
}

object Orbit {
    def decode(line: String): Orbit = {
        line.split("\\)") match {
            case Array(foo, bar) => Orbit(foo, bar)
        }
    }
}
case class Orbit(centerPoint: ID, spinning: ID)
