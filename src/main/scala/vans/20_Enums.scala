package vans

import io.circe.generic.semiauto.*
import io.circe.{Encoder, Json}
import io.circe.syntax.*

object Test20:
  given Encoder[VanKind] = Encoder.encodeString.contramap(k => k.toString)

  enum Rental derives Encoder.AsObject:
    case Tent(people: Int, hasVestibule: Boolean)
    case Van(kind: VanKind, length: Double)
    case House(bedrooms: Int, location: (Double, Double))

  enum VanKind:
    case Medium, Large, Pickup, Minibus, Camper

  def capacity(r: Rental): Int = r match
    case Rental.Tent(p, _)  => p
    case Rental.Van(_, _)   => 4
    case Rental.House(b, _) => b * 2

  extension (r: Rental)
    def capacity2: Int = r match
      case Rental.Tent(p, _)  => p
      case Rental.Van(_, _)   => 4
      case Rental.House(b, _) => b * 2

@main def test20main() =
  import Test20.*
  println("Hello, world!")
  println((Rental.Tent(10, true): Rental).asJson)
  println((Rental.Van(VanKind.Pickup, 10.2): Rental).asJson)
  println((Rental.House(5, (10.2, 11.0)): Rental).asJson)

  println(capacity(Rental.Van(VanKind.Pickup, 10.2)))
  println(Rental.Van(VanKind.Pickup, 10.2).capacity2)
