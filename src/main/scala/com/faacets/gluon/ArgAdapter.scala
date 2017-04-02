package com.faacets.gluon

import java.math.BigInteger

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/** Exception thrown when a provided argument value cannot be adapted to the type expected by the Scala code.
  *
  * @param a        Argument adapter instance
  * @param argIndex Index of the argument
  * @param argName  Name of the argument
  * @param arg      Value provided from the external code
  * @param cause    Exception thrown during the conversion
  */
class ArgAdapterException(a: ArgAdapter[_], argIndex: Int, argName: String, arg: Any, cause: AdapterException) extends IllegalArgumentException(s"Argument $argName (#$argIndex): cannot adapt to ${a.name} the given $arg of class ${arg.getClass.getName}", cause)

/** Adapts an argument provided as a standard Java type to the type expected by the Scala code. */
class ArgAdapter[+A](val name: String, f: Any => A) {

  /** Converts the provided argument from a standard Java type to the Scala type A.
    * Catches exceptions and rethrows a [[ArgAdapterException]] for better error reporting.
    *
    * @param argIndex Index of the argument to convert (for error reporting)
    * @param argName  Name of the argument to convert (for error reporting)
    * @param arg      Value of the argument to convert
    * @return the converted value
    */
  def argTo(argIndex: Int, argName: String, arg: Any): A = try {
    f(arg)
  } catch {
    case ae: AdapterException => throw new ArgAdapterException(this, argIndex, argName, arg, ae)
  }

  /** Converts the argument provided in a standard Java type to the Scala type A. */
  def to(arg: Any): A = f(arg)

}

object ArgAdapter extends ArgAdapterInstances {

  /** Returns an implicit instance; shorthand for `implicitly[ArgAdapter[A]]` */
  def apply[A](implicit ev: ArgAdapter[A]): ArgAdapter[A] = ev

  /** Creates an ArgAdapter from the name of the type and a conversion function.
    * The function should throw [[AdapterException]] on conversion failure (this exception
    * type is wrapped with additional information when propagated, which is not the case
    * for other exception types).
    */
  def apply[A](name: String)(f: Any => A): ArgAdapter[A] = new ArgAdapter[A](name, f)

}
