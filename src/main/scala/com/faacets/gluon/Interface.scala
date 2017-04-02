package com.faacets.gluon

import scala.annotation.varargs

class InterfaceCallException(funName: String, argNames: Seq[String], args: Seq[Any], cause: Exception) extends Exception(s"Exception in call to $funName(${InterfaceCallException.formatArgs(argNames, args)})", cause)

object InterfaceCallException {

  def formatArgs(argNames: Seq[String], args: Seq[Any]): String =
    (argNames zip args).map { case (n, a) => s"$n = $a" }.mkString(", ")

}

trait Interface {

  @varargs def call(args: Any*): Any

}

object Interface {

  type GenResAdapter[R] = ResAdapter[R, Any]

  def GenResAdapter[R](implicit ev: ResAdapter[R, Any]): GenResAdapter[R] = ev

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

  def compile(code: Array[String]): Interface = compile(code.mkString("\n"))

  def compile(code: String): Interface = synchronized {
    codeCache.get(code) match {
      case Some(i) => i
      case None =>
        val i = compileNoCache(code)
        codeCache = codeCache + (code -> i)
        i
    }
  }

  def compileNoCache(code: Array[String]): Interface = compileNoCache(code.mkString("\n"))

  def compileNoCache(code: String): Interface = {
    val tree = tb.parse(code)
    val res = tb.eval(tree).asInstanceOf[Interface]
    res
  }

  val squareCode = """
import com.faacets.gluon._
Interface[Double, Double]("square", "x") { x => x * x }
"""

  val powCode = """
import com.faacets.gluon._
Interface[Double, Int, Double]("square", "x", "exp") { (x, exp) => java.lang.Math.pow(x, exp) }
"""

  val sumCode = """
import com.faacets.gluon._
Interface[Seq[Double], Double]("sum", "seq") { seq => seq.sum }
"""

}
