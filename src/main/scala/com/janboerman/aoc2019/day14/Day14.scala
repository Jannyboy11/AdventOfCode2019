package com.janboerman.aoc2019.day14

import java.util.regex.Pattern


import scala.collection.mutable
import scala.io.Source

object Day14 extends App {

    type Element = String

    val fileName = "src/main/resources/day14input.txt"
    val reactions: Seq[Reaction] = Source.fromFile(fileName).getLines().map(Reaction.parse).toList
//    val reactions: Seq[Reaction] = Seq(
//        Reaction(Chemical("A", 10), Seq(Chemical("ORE", 10))),
//        Reaction(Chemical("B", 1), Seq(Chemical("ORE", 1))),
//        Reaction(Chemical("C", 1), Seq(Chemical("A", 7), Chemical("B", 1))),
//        Reaction(Chemical("D", 1), Seq(Chemical("A", 7), Chemical("C", 1))),
//        Reaction(Chemical("E", 1), Seq(Chemical("A", 7), Chemical("D", 1))),
//        Reaction(Chemical("FUEL", 1), Seq(Chemical("A", 7), Chemical("E", 1))))

    val reactionMap = new mutable.HashMap[Element, Reaction]()
    for (reaction <- reactions) {
        reactionMap.getOrElseUpdate(reaction.product.element, reaction)
    }

    val (result1, state) = getOreRequirements(Chemical("FUEL", 1), Map.empty[Element, BigInt])
    println(result1) //1582325

    val oneTrillion = BigInt(1_000_000_000_000L)
    var searching = true
    var refining = false
    var lastOre = BigInt(0)
    var diff = BigInt(1)
    var nextFuel = BigInt(1)
    var lastFuel = BigInt(0)
    var step = diff
    while (searching) {
        val fuel = Chemical("FUEL", nextFuel)
        val (newOre, _) = getOreRequirements(fuel, Map.empty[Element, BigInt])

        diff = (newOre - lastOre).abs
        if (!refining) {
            step = diff
            if (newOre < oneTrillion) {
                nextFuel *= BigInt(2)
            } else {
                refining = true
            }
        } else {
            if (newOre <= oneTrillion) {
                val oneMoreFuel = Chemical("FUEL", nextFuel + BigInt(1))
                val (oneMoreFuelOre, _) = getOreRequirements(oneMoreFuel, Map.empty)
                if (oneMoreFuelOre > oneTrillion) {
                    searching = false
                } else {
                    nextFuel += step
                }
            } else {
                //need to decrease the fuel we require.
                step = step / BigInt(2)
                if (step == BigInt(0)) step = BigInt(1)
                nextFuel = lastFuel - step
            }
        }

        lastOre = newOre
        lastFuel = nextFuel
    }
    println(lastFuel) //2267486




    def getOreRequirements(chemical: Chemical, reserves: Map[Element, BigInt]): (BigInt, Map[Element, BigInt]) = {
        //assume the amounts of the chemical have no common divisor.
        val element = chemical.element
        val howMany = chemical.howMany

        if ("ORE".equals(element)) return (howMany, reserves)

        def removeReserve(element: Element, howMany: BigInt, from: Map[Element, BigInt]): (Map[Element, BigInt], BigInt) = {
            val amount = from.getOrElse(element, BigInt(0))
            if (amount > howMany) {
                val leftOverAmount = amount - howMany
                val resMap = from.updated(element, leftOverAmount)
                (resMap, BigInt(0))
            } else {
                val howManyWeStillNeed = howMany - amount
                val resMap = from.removed(element)
                (resMap, howManyWeStillNeed)
            }
        }

        val (updatedSupply, updatedHowManyWeNeed) = removeReserve(element, howMany, reserves)
        if (updatedHowManyWeNeed <= BigInt(0)) return (BigInt(0), updatedSupply)

        val reaction = reactionMap(element)
        val factor = if (reaction.product.howMany < updatedHowManyWeNeed) {
            var divisor = updatedHowManyWeNeed / reaction.product.howMany
            if (updatedHowManyWeNeed % reaction.product.howMany != BigInt(0)) divisor += BigInt(1)
            divisor
        } else BigInt(1)
        val updatedReaction = Reaction(Chemical(reaction.product.element, reaction.product.howMany * factor),
            reaction.requirements.map({case Chemical(element, howMany) => Chemical(element, howMany * factor)}))

        val currentReserve = updatedSupply.getOrElse(element, BigInt(0))
        val newLeftOver = currentReserve + updatedReaction.product.howMany - updatedHowManyWeNeed

        var ore = BigInt(0)
        var state = updatedSupply.updated(element, newLeftOver)
        for (chem <- updatedReaction.requirements) {
            val (reqOre, newState) = getOreRequirements(chem, state)
            ore += reqOre
            state = newState
        }
        (ore, state)
    }

}
import Day14.{Element}

object Chemical {
    def apply(element: Element, howMany: Int): Chemical = Chemical(element, BigInt(howMany))
}
case class Chemical(element: Element, howMany: BigInt)

object Reaction {
    private val pattern = Pattern.compile("\\d+ [A-Z]+")

    def parse(string: String): Reaction = {
        val matcher = pattern.matcher(string)
        val chemicals = new mutable.ListBuffer[Chemical]();
        while (matcher.find()) {
            val Array(count, element) = matcher.group().split(" ")
            chemicals.addOne(Chemical(element, count.toInt))
        }
        val product = chemicals.last
        val requirements = chemicals.init
        Reaction(product, requirements.toSeq)
    }
}
case class Reaction(product: Chemical, requirements: Seq[Chemical]) {
    override def toString: String = {
        val requirements = this.requirements.map(c => s"${c.howMany} ${c.element}").mkString(", ")
        val result = s"${product.howMany} ${product.element}"
        s"$requirements => $result"
    }
}
