package com.janboerman.aoc2019.day8

import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters._

object Day8 extends App {

    val fileName = "src/main/resources/day8input.txt"
    val digits = Source.fromFile(fileName).getLines().next.chars().map(_ - '0').iterator().asScala.map(_.intValue()).toIndexedSeq

    var wide = 25
    var tall = 6


    var picture = digits.grouped(wide * tall).toIndexedSeq
    //picture.foreach(println)

    val layer = picture.minBy(_.count(_ == 0))
    val result1 = layer.count(_ == 1) * layer.count(_ == 2)
    println(result1)

//    wide = 2
//    tall = 2
//    picture = IndexedSeq(IndexedSeq(0, 2, 2, 2), IndexedSeq(1, 1, 2, 2), IndexedSeq(2, 2, 1, 2), IndexedSeq(0, 0, 0, 0))

    var image = new mutable.ListBuffer[Int]()
    var layerIndex = 0
    while (layerIndex < picture.size) {
        val pixelIndex = layerIndex
        val layer = picture(layerIndex)
        var pixel = layer(pixelIndex)
        var newLayerIndex = layerIndex
        while (pixel == 2) {
            newLayerIndex += 1
            pixel = picture(newLayerIndex)(pixelIndex)
        }

        image.append(pixel)

        layerIndex += 1
    }

    picture.foreach(println)
    println()
    println(image.toVector)
    println()

    //result2
    for (row <- image.grouped(wide)) {
        val line = row
            .map {
            case 0 => '\u2591'
            case 1 => '\u2588'
        }.mkString
        println(line)
    }
}
