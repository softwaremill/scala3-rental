package vans

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.openapi.circe.yaml.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.SwaggerUI
import sttp.tapir.json.circe.*

import scala.concurrent.ExecutionContext
import scala.util.Random

object Test50:
  given Encoder[VanKind] = Encoder.encodeString.contramap(k => k.toString)
  given Schema[VanKind] = Schema.string
  given Schema[(Double, Double)] =
    Schema.schemaForArray[Double].as[(Double, Double)]

  enum Rental derives Encoder.AsObject, Decoder, Schema:
    case Tent(people: Int, hasVestibule: Boolean)
    case Van(kind: VanKind, length: Units.Meters)
    case House(bedrooms: Int, location: (Double, Double))

  enum VanKind:
    case Medium, Large, Pickup, Minibus, Camper

  extension (r: Rental)
    def capacity: Int = r match
      case Rental.Tent(p, _)  => p
      case Rental.Van(_, _)   => 4
      case Rental.House(b, _) => b * 2

  enum RentalError derives Encoder.AsObject, Decoder, Schema:
    case OutOfStock

  case class RentalWithId(id: String, rental: Rental)
      derives Encoder.AsObject,
        Decoder,
        Schema

@main def test50main() =
  import Test50.*

  var rentalIds = Set[String]()

  val rentTent: ServerEndpoint[Int, RentalError, RentalWithId, Any, IO] =
    endpoint.post
      .in("rent" / "tent")
      .in(query[Int]("people"))
      .errorOut(jsonBody[RentalError])
      .out(jsonBody[RentalWithId])
      .serverLogic(people =>
        IO {
          if (people > 6) then Left(RentalError.OutOfStock)
          else
            val id =
              (for (i <- 1 to 10) yield Random.nextPrintableChar()).mkString
            rentalIds = rentalIds + id
            Right(
              RentalWithId(
                id,
                Rental.Tent(people, hasVestibule = Random.nextBoolean())
              )
            )
        }
      )

  val checkRental: ServerEndpoint[String, Unit, Unit, Any, IO] = endpoint.get
    .in("rent" / "tent")
    .in(query[String]("id"))
    .errorOut(statusCode(StatusCode.NotFound))
    .serverLogic(id =>
      IO {
        if (rentalIds.contains(id)) then Right(()) else Left(())
      }
    )

  val docs = OpenAPIDocsInterpreter()
    .serverEndpointsToOpenAPI(
      List(rentTent, checkRental),
      "Hello, World!",
      "1.0"
    )
    .toYaml

  val routes: HttpRoutes[IO] =
    Http4sServerInterpreter[IO]().toRoutes(
      List(rentTent, checkRental) ++ SwaggerUI[IO](docs)
    )

  implicit val runtime: IORuntime = IORuntime.global

  BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.Implicits.global)
    .bindHttp(8080, "localhost")
    .withHttpApp(
      Router("/" -> routes).orNotFound
    )
    .resource
    .use(_ => IO.never)
    .as(ExitCode.Success)
    .unsafeRunSync()
