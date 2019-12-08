package com.janboerman.aoc2019.day8

import scala.io.Source
import scala.jdk.CollectionConverters._

object Day8 extends App {

    val fileName = "src/main/resources/day8input.txt"
    val digits = Source.fromFile(fileName).getLines().next.chars().map(_ - '0').iterator().asScala.map(_.intValue()).toIndexedSeq

    val wide = 25
    val tall = 6
    val picture = digits.grouped(wide * tall).toIndexedSeq

    //picture.foreach(println)

    val layer = picture.minBy(_.count(_ == 0))
    val result1 = layer.count(_ == 1) * layer.count(_ == 2)
    println(result1)

    //wide = 2
    //tall = 2
    //picture = IndexedSeq(IndexedSeq(0, 2, 2, 2), IndexedSeq(1, 1, 2, 2), IndexedSeq(2, 2, 1, 2), IndexedSeq(0, 0, 0, 0))

    //result2
    var image = picture.foldLeft[Iterable[Int]](List.fill(wide * tall)(2))(mergeLayer)
    for (row <- image.grouped(wide)) {
        val line = row
            .map {
            case 0 => '\u2591'
            case 1 => '\u2588'
        }.mkString
        println(line)
    }

    def mergeLayer(layer1: Iterable[Int], layer2: Iterable[Int]): Iterable[Int] = {
        layer1.zip(layer2).map {case (p1, p2) => if (p1 != 2) p1 else p2}
    }
}
