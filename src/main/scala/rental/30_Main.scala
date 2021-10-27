package rental

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.openapi.circe.yaml.*
import sttp.tapir.swagger.SwaggerUI
import sttp.tapir.json.circe.*
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.http4s.blaze.server.BlazeServerBuilder
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import scala.util.Random

case class Person(name: String, age: Int) derives Encoder.AsObject

object Units:
  opaque type Meters = Double

  object Meters:
    def apply(d: Double): Meters = d

  extension (x: Meters)
    def show: String =
      val m = Math.floor(x).toInt
      val cm = Math.floor(x * 100).toInt % 100
      s"${m}m${cm}cm"

  given Encoder[Units.Meters] = Encoder.encodeDouble.contramap(identity)
  given Decoder[Units.Meters] = Decoder.decodeDouble.map(Meters(_))
  given Schema[Units.Meters] = Schema.schemaForDouble

enum Rental derives Encoder.AsObject, Decoder, Schema:
  case Tent(people: Int, hasVestibule: Boolean)
  case Van(kind: VanKind, length: Units.Meters)
  case House(bedrooms: Int, location: (Double, Double))

given Encoder[VanKind] = Encoder.encodeString.contramap(k => k.toString)
given Schema[VanKind] = Schema.string
given Schema[(Double, Double)] =
  Schema.schemaForArray[Double].as[(Double, Double)]

enum VanKind:
  case Medium, Large, Pickup, Minibus, Camper

extension (r: Rental)
  def capacity: Int = r match
    case Rental.Tent(p, _)  => p
    case Rental.Van(_, _)   => 4
    case Rental.House(b, _) => b * 2

extension (n: Int) def meters: Units.Meters = Units.Meters(n)

@main def helloWorld() =

  // GET /hello?name=...
  val helloWorld: Endpoint[String, Unit, String, Any] =
    endpoint.get.in("hello").in(query[String]("name")).out(stringBody)

  val helloWorldRoutes: HttpRoutes[IO] =
    Http4sServerInterpreter[IO]().toRoutes(helloWorld)(name =>
      IO(Right(s"Hello, $name!"))
    )

  //

  enum RentalError derives Encoder.AsObject, Decoder, Schema:
    case OutOfStock
    case TooManyPeople

  case class RentalWithId(id: String, rental: Rental)
      derives Encoder.AsObject,
        Decoder,
        Schema

  val rentTent =
    endpoint.post
      .in("rent" / "tent")
      .in(query[Int]("people"))
      .errorOut(jsonBody[RentalError])
      .out(jsonBody[RentalWithId])
      // Int => IO[Either[RentalError, RentalWithId]]
      .serverLogic(people =>
        IO {
          if (people > 10) then Left(RentalError.TooManyPeople)
          else if (people > 6) then Left(RentalError.OutOfStock)
          else
            val id =
              (for (i <- 1 to 10) yield Random.nextPrintableChar()).mkString
            Right(RentalWithId(id, Rental.Tent(people, hasVestibule = true)))
        }
      )

  val rentalRoutes = Http4sServerInterpreter[IO]().toRoutes(rentTent)

  //

  val docs: String = OpenAPIDocsInterpreter()
    .toOpenAPI(List(helloWorld, rentTent.endpoint), "Summer rentals!", "1.0")
    .toYaml
  val swaggerUIRoute =
    Http4sServerInterpreter[IO]().toRoutes(SwaggerUI[IO](docs))

  given IORuntime = IORuntime.global

  // <+> requires: import io.circe.syntax.*

  BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.Implicits.global)
    .bindHttp(8080, "localhost")
    .withHttpApp(
      Router(
        "/" -> (helloWorldRoutes <+> rentalRoutes <+> swaggerUIRoute)
      ).orNotFound
    )
    .resource // -> cats-effect Resource[Server]
    .use(_ => IO.never)
    .unsafeRunSync()
