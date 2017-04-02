package com.faacets.gluon

import scala.annotation.varargs

/** Exception thrown when the interface call could not proceed, either because
  * the arguments could not be converted, the function call itself throwed, or
  * the result could not be converted back.
  *
  * @param funName  Name of the interface function
  * @param argNames Names of the arguments
  * @param args     Provided values for the arguments
  * @param cause    Cause of the failure (exception which caused the rethrow)
  */
class InterfaceCallException(funName: String, argNames: Seq[String], args: Seq[Any], cause: Exception) extends Exception(s"Exception in call to $funName(${InterfaceCallException.formatArgs(argNames, args)})", cause)

object InterfaceCallException {

  /** Formats a pair of name-arg lists in the manner "x1 = 1, x2 = 4, ..." */
  def formatArgs(argNames: Seq[String], args: Seq[Any]): String =
    (argNames zip args).map { case (n, a) => s"$n = $a" }.mkString(", ")

}

/** Generic interface for wrapped Scala code. The call method should be
  * prefered, but call0 ... call4 can be used to inform better the host
  * language about the expected number of arguments. The latter should be
  * used when using cell arrays to represent a single parameter in Matlab.
  */
trait Interface {

  /** Call form with a variable argument list. */
  @varargs def call(args: Any*): Any

  /** Call without arguments. */
  def call0(): Any = call()

  /** Call with a single argument. */
  def call1(arg0: Any): Any = call(arg0)

  /** Call with two arguments. */
  def call2(arg0: Any, arg1: Any): Any = call(arg0, arg1)

  /** Call with three arguments. */
  def call3(arg0: Any, arg1: Any, arg2: Any): Any = call(arg0, arg1, arg2)

  /** Call with four arguments. */
  def call4(arg0: Any, arg1: Any, arg2: Any, arg3: Any): Any = call(arg0, arg1, arg2, arg3)

}

object Interface {

  type GenResAdapter[R] = ResAdapter[R, Any]

  def GenResAdapter[R](implicit ev: ResAdapter[R, Any]): GenResAdapter[R] = ev

  /** Creates an interface instance with zero arguments.
    *
    * @param funName Function name
    * @param f       Function
    * @tparam R      Return type
    */
  def apply[R:GenResAdapter](funName: String)(f: => R): Interface = new Interface {
    @varargs def call(args: Any*): Any = {
      try {
        GenResAdapter[R].from(f)
      } catch {
        case e: Exception => throw new InterfaceCallException(funName, Seq(), args, e)
      }
    }

  }

  /** Creates an interface instance with a single argument.
    *
    * @param funName Function name
    * @param name0   First argument name
    * @param f       Function
    * @tparam A0     First argument type
    * @tparam R      Return type
    */
  def apply[A0:ArgAdapter, R:GenResAdapter](funName: String, name0: String)
                                        (f: A0 => R): Interface = new Interface {
    @varargs def call(args: Any*): Any = {
      try {
        val a0 = ArgAdapter[A0].argTo(0, name0, args(0))
        GenResAdapter[R].from(f(a0))
      } catch {
        case e: Exception => throw new InterfaceCallException(funName, Seq(name0), args, e)
      }
    }

  }

  /** Creates an interface instance with a two arguments.
    *
    * @param funName Function name
    * @param name0   First argument name
    * @param name1   Second argument name
    * @param f       Function
    * @tparam A0     First argument type
    * @tparam A1     Second argument type
    * @tparam R      Return type
    */
  def apply[A0:ArgAdapter, A1:ArgAdapter, R:GenResAdapter](funName: String, name0: String, name1: String)
                                                       (f: (A0, A1) => R): Interface = new Interface {

    @varargs def call(args: Any*): Any = {
      try {
        val a0 = ArgAdapter[A0].argTo(0, name0, args(0))
        val a1 = ArgAdapter[A1].argTo(1, name1, args(1))
        GenResAdapter[R].from(f(a0, a1))
      } catch {
        case e: Exception => throw new InterfaceCallException(funName, Seq(name0, name1), args, e)
      }
    }

  }

  def apply[A0:ArgAdapter, A1:ArgAdapter, A2:ArgAdapter, R:GenResAdapter](funName: String, name0: String, name1: String, name2: String)
                                                                      (f: (A0, A1, A2) => R): Interface = new Interface {

    @varargs def call(args: Any*): Any = {
      try {
        val a0 = ArgAdapter[A0].argTo(0, name0, args(0))
        val a1 = ArgAdapter[A1].argTo(1, name1, args(1))
        val a2 = ArgAdapter[A2].argTo(2, name1, args(2))
        GenResAdapter[R].from(f(a0, a1, a2))
      } catch {
        case e: Exception => throw new InterfaceCallException(funName, Seq(name0, name1, name2), args, e)
      }
    }

  }

  def apply[A0:ArgAdapter, A1:ArgAdapter, A2:ArgAdapter, A3:ArgAdapter, R:GenResAdapter](funName: String, name0: String, name1: String, name2: String, name3: String)
                                                                                     (f: (A0, A1, A2, A3) => R): Interface = new Interface {

    @varargs def call(args: Any*): Any = {
      try {
        val a0 = ArgAdapter[A0].argTo(0, name0, args(0))
        val a1 = ArgAdapter[A1].argTo(1, name1, args(1))
        val a2 = ArgAdapter[A2].argTo(2, name1, args(2))
        val a3 = ArgAdapter[A3].argTo(3, name1, args(3))
        GenResAdapter[R].from(f(a0, a1, a2, a3))
      } catch {
        case e: Exception => throw new InterfaceCallException(funName, Seq(name0, name1, name2, name3), args, e)
      }
    }

  }
  private var codeCache: Map[String, Interface] = Map.empty[String, Interface]

  import scala.reflect.runtime.{universe => ru}
  import scala.tools.reflect.ToolBox

  //  Scala compiler tool box
  private val tb = ru.runtimeMirror(this.getClass.getClassLoader).mkToolBox()

  /** Compiles an array of lines of code to an Interface. */
  def compile(code: Array[String]): Interface = compile(code.mkString("\n"))

  /** Compiles source code to an Interface. */
  def compile(code: String): Interface = synchronized {
    codeCache.get(code) match {
      case Some(i) => i
      case None =>
        val i = compileNoCache(code)
        codeCache = codeCache + (code -> i)
        i
    }
  }

  /** Same as compile, but does not cache the result. */
  def compileNoCache(code: Array[String]): Interface = compileNoCache(code.mkString("\n"))

  /** Same as compile, but does not cache the result. */
  def compileNoCache(code: String): Interface = {
    val tree = tb.parse(code)
    val res = tb.eval(tree).asInstanceOf[Interface]
    res
  }

  /** Example code that squares its argument. */
  val squareCode = """
import com.faacets.gluon._
Interface[Double, Double]("square", "x") { x => x * x }
"""

  /** Example code that perform the pow operation. */
  val powCode = """
import com.faacets.gluon._
Interface[Double, Int, Double]("square", "x", "exp") { (x, exp) => java.lang.Math.pow(x, exp) }
"""

  /** Example code that sums elements in a sequence. */
  val sumCode = """
import com.faacets.gluon._
Interface[Seq[Double], Double]("sum", "seq") { seq => seq.sum }
"""

}
