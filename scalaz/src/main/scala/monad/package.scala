package unfiltered
import scalaz._
import Scalaz._

package object monad extends RequestLoggers {
  def coalesce(args:Seq[String],split:Option[String => Seq[String]] = none):Option[Seq[String]] =
    (args.foldLeft(Vector[String]()) {
      (b:Vector[String],v:String) => v.trim match {
        case "" => b
        case s0 => (split map ((f:String => Seq[String]) => b ++ (f(s0) map (_.trim) filterNot (!_.isEmpty)))) | (b :+ (s0))
      }
    }) match {
      case Vector() => none
      case vs => some(vs)
    }
}
