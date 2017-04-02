package com.faacets.gluon

import org.scalatest.{FunSuite, Matchers}

class GluonSuite extends FunSuite with Matchers {

  test("Arg adapters") {
    ArgAdapter[Int].to(1) shouldBe 1
    ArgAdapter[Int].to(1.0) shouldBe 1

    // Long which can be represented exactly as Double can be converted
    ArgAdapter[Double].to(100L) shouldBe 100.0

    assertThrows[AdapterException] {
      ArgAdapter[Double].to(Long.MaxValue - 1) shouldBe 1
    }
  }

  test("Pow") {
    val powCode =
      """
import com.faacets.gluon._
Interface[Double, Int, Double]("square", "x", "exp") { (x, exp) => java.lang.Math.pow(x, exp) }
"""
    Interface.compile(powCode).call(3.0, 2) shouldBe 9.0
  }

}
