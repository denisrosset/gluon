package com.faacets.gluon

import scala.collection.{GenMap, GenTraversable, mutable}
import scala.util.Success

/** Pretty-printer adapted from Scalatest/Scalactic prettifier. */
object Printer {

  /** XML formatting is separate to provide ScalaJS support. */
  object FormatXML {
    def unapply(o: Any): Option[String] = o match {
      case anXMLNodeSeq: xml.NodeSeq => Some(anXMLNodeSeq.toString)
      case anXMLNodeBuffer: xml.NodeBuffer => Some(xml.NodeSeq.fromSeq(anXMLNodeBuffer).toString)
      case _ => None
    }
  }

  /** Java collections support is separate to provide ScalaJS support. */
  object FormatJavaCol {
    def unapply(o: Any): Option[String] = o match {
      case javaCol: java.util.Collection[_] =>
        // By default java collection follows http://download.java.net/jdk7/archive/b123/docs/api/java/util/AbstractCollection.html#toString()
        // let's do our best to prettify its element when it is not overriden
        import scala.collection.JavaConverters._
        val theToString = javaCol.toString
        if (theToString.startsWith("[") && theToString.endsWith("]"))
          Some("[" + javaCol.iterator().asScala.map(apply(_)).mkString(", ") + "]")
        else
          Some(theToString)
      case javaMap: java.util.Map[_, _] =>
        // By default java map follows http://download.java.net/jdk7/archive/b123/docs/api/java/util/AbstractMap.html#toString()
        // let's do our best to prettify its element when it is not overriden
        import scala.collection.JavaConverters._
        val theToString = javaMap.toString
        if (theToString.startsWith("{") && theToString.endsWith("}"))
          Some("{" + javaMap.entrySet.iterator.asScala.map { entry =>
            apply(entry.getKey) + "=" + apply(entry.getValue)
          }.mkString(", ") + "}")
        else
          Some(theToString)
      case _ => None
    }
  }

  /** Provides a best effort string representation of the given argument.
    * Especially important when printing Java arrays.
    */
  def apply(o: Any): String = try {
    o match {
      case null => "null"
      case aUnit: Unit => "<(), the Unit value>"
      case aString: String => "\"" + aString + "\""
      case aStringWrapper: scala.collection.immutable.StringOps => "\"" + aStringWrapper + "\""
      case aChar: Char => "\'" + aChar + "\'"
      case Some(e) => "Some(" + apply(e) + ")"
      case Success(e) => "Success(" + apply(e) + ")"
      case Left(e) => "Left(" + apply(e) + ")"
      case Right(e) => "Right(" + apply(e) + ")"
      case anArray: Array[_] => "Array(" + (anArray map apply).mkString(", ") + ")"
      case aWrappedArray: mutable.WrappedArray[_] => "Array(" + (aWrappedArray map apply).mkString(", ") + ")"
      case aGenMap: GenMap[_, _] =>
        aGenMap.stringPrefix + "(" +
          (aGenMap.toIterator.map { case (key, value) => // toIterator is needed for consistent ordering
            apply(key) + " -> " + apply(value)
          }).mkString(", ") + ")"
      case FormatXML(string) => string
      case aGenTraversable: GenTraversable[_] =>
        val isSelf =
          if (aGenTraversable.size == 1) {
            aGenTraversable.head match {
              case ref: AnyRef => ref eq aGenTraversable
              case other => other == aGenTraversable
            }
          }
          else
            false
        if (isSelf)
          aGenTraversable.toString
        else
          aGenTraversable.stringPrefix + "(" + aGenTraversable.toIterator.map(apply(_)).mkString(", ") + ")" // toIterator is needed for consistent ordering
      case FormatJavaCol(string) => string
      case anythingElse => anythingElse.toString
    }
  } catch {
    // This is in case of crazy designs like the one for scala.xml.Node. We handle Node
    // specially above, but in case someone else creates a collection whose iterator
    // returns itself, which will cause infinite recursion, at least we'll pop out and
    // give them a string back.
    case _: StackOverflowError => o.toString
  }

}
