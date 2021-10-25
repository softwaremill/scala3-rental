package rental.done

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, Resource}
import cats.syntax.all.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.{Router, Server}
import sttp.model.StatusCode
import sttp.client3.*
import sttp.client3.httpclient.fs2.HttpClientFs2Backend
import sttp.client3.circe.*
import sttp.tapir.*
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.openapi.circe.yaml.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.SwaggerUI
import sttp.tapir.json.circe.*

import scala.concurrent.ExecutionContext
import scala.util.Random

object Test60:
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

class Test60Logic(backend: SttpBackend[IO, Any]) {
  import Test60.*

  var rentalIds = Set[String]()

  def checkProvider1: IO[String] = {
    case class Provider1Response(origin: String) derives Decoder
    basicRequest
      .get(uri"https://httpbin.org/ip")
      .response(asJson[Provider1Response].getRight.map(_.origin))
      .send(backend)
      .map(_.body)
  }

  def checkProvider2: IO[String] = {
    case class Provider2Response(`user-agent`: String) derives Decoder
    basicRequest
      .get(uri"https://httpbin.org/user-agent")
      .response(asJson[Provider2Response].getRight.map(_.`user-agent`))
      .send(backend)
      .map(_.body)
  }

  def rent(people: Int): IO[Either[RentalError, RentalWithId]] =
    import cats.syntax.parallel.*

    def print(s: String): IO[Unit] = IO(println(s))

    val check: IO[(String, String)] = checkProvider1.parProduct(checkProvider2)

    val generateId = IO {
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

    check.map(_.toString).flatMap(print).flatMap(_ => generateId)

  def check(id: String): IO[Either[Unit, Unit]] = IO {
    if (rentalIds.contains(id)) then Right(()) else Left(())
  }
}

class Test60RentalEndpoints(logic: Test60Logic) {
  import Test60.*

  val rentTent: ServerEndpoint[Int, RentalError, RentalWithId, Any, IO] =
    endpoint.post
      .in("rent" / "tent")
      .in(query[Int]("people"))
      .errorOut(jsonBody[RentalError])
      .out(jsonBody[RentalWithId])
      .serverLogic(logic.rent)

  val checkRental: ServerEndpoint[String, Unit, Unit, Any, IO] = endpoint.get
    .in("rent" / "tent")
    .in(query[String]("id"))
    .errorOut(statusCode(StatusCode.NotFound))
    .serverLogic(logic.check)

  val all = List(rentTent, checkRental)
}

class Test60Routes(endpoints: Test60RentalEndpoints) {}

@main def test60main() =
  import Test60.*

  implicit val runtime: IORuntime = IORuntime.global

  val clientResource = HttpClientFs2Backend.resource[IO]()
  def routesWithDocs(endpoints: Test60RentalEndpoints): HttpRoutes[IO] =
    val docs = OpenAPIDocsInterpreter()
      .serverEndpointsToOpenAPI(endpoints.all, "Hello, World!", "1.0")
      .toYaml

    Http4sServerInterpreter[IO]().toRoutes(
      endpoints.all ++ SwaggerUI[IO](docs)
    )

  def serverResource(routes: HttpRoutes[IO]): Resource[IO, Server] =
    BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.Implicits.global)
      .bindHttp(8080, "localhost")
      .withHttpApp(
        Router("/" -> routes).orNotFound
      )
      .resource

  clientResource
    .flatMap { backend =>
      val logic = new Test60Logic(backend)
      val endpoints = new Test60RentalEndpoints(logic)
      val routes = routesWithDocs(endpoints)
      serverResource(routes)
    }
    .use(_ => IO.never)
    .as(ExitCode.Success)
    .unsafeRunSync()
