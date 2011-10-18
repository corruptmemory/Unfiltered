package unfiltered.monad
import scalaz._
import Scalaz._
import unfiltered.request._
import scala.util.control.Exception._

abstract class Conversion[T] {
  def to(in:String):ValidationNEL[String,T]
  def toSeq(in:Seq[String]):ValidationNEL[String,Seq[T]]
}

object Conversion {
  class ValidationPimp[E,A](v:Validation[E,A]) {
    def mapFail[B](f:E => B):Validation[B,A] = v.fold(success = s => s.success,
                                                      failure = x => f(x).fail)
    def onFail[B](f:E => Validation[B,A]):Validation[B,A] = v.fold(success = s => s.success,
                                                                   failure = x => f(x))
}

  implicit def ValidationPimp[E,A](v:Validation[E,A]):ValidationPimp[E,A] = new ValidationPimp(v)
}

object DefaultConversions {
  import Conversion._

  implicit val stringConversions = new Conversion[String] {
    def to(in:String):ValidationNEL[String,String] = in.success
    def toSeq(in:Seq[String]):ValidationNEL[String,Seq[String]] = in.success
  }

  implicit val byteConversions = new Conversion[Byte] {
    def to(in:String):ValidationNEL[String,Byte] = in.parseByte.mapFail[String](_ => "Cannot convert %s to Byte".format(in)).liftFailNel
    def toSeq(in:Seq[String]):ValidationNEL[String,Seq[Byte]] = in.traverse[({type λ[α]=ValidationNEL[String, α]})#λ,Byte](to(_))
  }

  implicit val intConversions = new Conversion[Int] {
    def to(in:String):ValidationNEL[String,Int] = in.parseInt.mapFail[String](_ => "Cannot convert %s to Int".format(in)).liftFailNel
    def toSeq(in:Seq[String]):ValidationNEL[String,Seq[Int]] = in.traverse[({type λ[α]=ValidationNEL[String, α]})#λ,Int](to(_))
  }

  implicit val shortConversions = new Conversion[Short] {
    def to(in:String):ValidationNEL[String,Short] = in.parseShort.mapFail[String](_ => "Cannot convert %s to Short".format(in)).liftFailNel
    def toSeq(in:Seq[String]):ValidationNEL[String,Seq[Short]] = in.traverse[({type λ[α]=ValidationNEL[String, α]})#λ,Short](to(_))
  }

  implicit val longConversions = new Conversion[Long] {
    def to(in:String):ValidationNEL[String,Long] = in.parseLong.mapFail[String](_ => "Cannot convert %s to Long".format(in)).liftFailNel
    def toSeq(in:Seq[String]):ValidationNEL[String,Seq[Long]] = in.traverse[({type λ[α]=ValidationNEL[String, α]})#λ,Long](to(_))
  }

  implicit val floatConversions = new Conversion[Float] {
    def to(in:String):ValidationNEL[String,Float] = in.parseFloat.mapFail[String](_ => "Cannot convert %s to Float".format(in)).liftFailNel
    def toSeq(in:Seq[String]):ValidationNEL[String,Seq[Float]] = in.traverse[({type λ[α]=ValidationNEL[String, α]})#λ,Float](to(_))
  }

  implicit val doubleConversions = new Conversion[Double] {
    def to(in:String):ValidationNEL[String,Double] = in.parseDouble.mapFail[String](_ => "Cannot convert %s to Double".format(in)).liftFailNel
    def toSeq(in:Seq[String]):ValidationNEL[String,Seq[Double]] = in.traverse[({type λ[α]=ValidationNEL[String, α]})#λ,Double](to(_))
  }

  implicit val boolConversions = new Conversion[Boolean] {
    def to(in:String):ValidationNEL[String,Boolean] = in.parseBoolean.mapFail[String](_ => "Cannot convert %s to Boolean".format(in)).liftFailNel
    def toSeq(in:Seq[String]):ValidationNEL[String,Seq[Boolean]] = in.traverse[({type λ[α]=ValidationNEL[String, α]})#λ,Boolean](to(_))
  }
}
