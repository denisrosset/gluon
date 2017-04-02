package com.faacets.gluon

import java.math.BigInteger

import scala.reflect.ClassTag

trait ResAdapterInstances {

  implicit def seq[A, B:ClassTag](implicit ev: ResAdapter[A, B]): ResAdapter[Seq[A], Array[B]] =
    ResAdapter[Seq[A], Array[B]]("Seq["+ResAdapter[A, B].name+"]") {
      seq => seq.map(a => ResAdapter[A, B].from(a)).toArray
    }

  implicit val long: ResAdapter[Long, Long] = ResAdapter[Long, Long]("Long")(identity)

  implicit val int: ResAdapter[Int, Int] = ResAdapter[Int, Int]("Int")(identity)

  implicit val double: ResAdapter[Double, Double] = ResAdapter[Double, Double]("Double")(identity)

  implicit val bigInt: ResAdapter[BigInt, BigInteger] = ResAdapter[BigInt, BigInteger]("BigInt")(_.bigInteger)

}
