package com.janboerman.aoc2019.util

object Math {

    def gcd(a: BigInt, b: BigInt): BigInt = {
        if (b == 0) a else gcd(b, a % b)
    }

    def lcm(a: BigInt, b: BigInt): BigInt = {
        (a * b) / gcd(a, b)
    }

}
