package rental.done

import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import sttp.tapir.Schema

object Units:
  opaque type Meters = Double

  object Meters:
    def apply(d: Double): Meters = d

  extension (x: Meters)
    def show: String = {
      val m = Math.floor(x).toInt
      val cm = Math.floor(x * 100).toInt % 100
      s"${m}m${cm}cm"
    }

  given Encoder[Units.Meters] = Encoder.encodeDouble.contramap(identity)

  // later
  given Decoder[Units.Meters] = Decoder.decodeDouble.map(Meters(_))
  given Schema[Units.Meters] = Schema.schemaForDouble

@main def test30() =
  import Units.*

  case class Person3(name: String, height: Units.Meters)
      derives Encoder.AsObject

  val x: Meters = Meters(10.139)
  println(x.show)

  println(Person3("John", Meters(1.91)).asJson)
