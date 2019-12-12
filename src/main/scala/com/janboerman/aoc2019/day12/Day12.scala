package com.janboerman.aoc2019.day12

import java.util.regex.Pattern

import scala.io.Source

object Day12 extends App {
    val fileName = "src/main/resources/day12input.txt"
    val numbers: Seq[Vec3d] = Source.fromFile(fileName).getLines().map(Vec3d.parse(_)).toList

    println(numbers)

}

object Vec3d {
    import scala.util.matching.Regex
    /*  <x=8, y=0, z=8>
        <x=0, y=-5, z=-10>
        <x=16, y=10, z=-5>
        <x=19, y=-10, z=-7>*/

    private val integerPattern = Pattern.compile("-?\\d+")

    def parse(string: String): Vec3d = {
        val matcher = integerPattern.matcher(string)
        val x = {matcher.find(); matcher.group().toInt}
        val y = {matcher.find(); matcher.group().toInt}
        val z = {matcher.find(); matcher.group().toInt}
        Vec3d(x, y, z)
    }
}

case class Vec3d(x: Int, y: Int, z: Int)
