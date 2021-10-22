package vans

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import cats.effect.unsafe.IORuntime
import io.circe.generic.semiauto.*
import io.circe.{Encoder, Json}
import io.circe.syntax.*
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.Router
import org.http4s.implicits.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.openapi.circe.yaml.*
import sttp.tapir.swagger.SwaggerUI
import sttp.tapir.*

import scala.concurrent.ExecutionContext

@main def test40() =
  val helloWorld: Endpoint[String, Unit, String, Any] =
    endpoint.get.in("hello").in(query[String]("name")).out(stringBody)

  val helloWorldRoutes: HttpRoutes[IO] =
    Http4sServerInterpreter[IO]().toRoutes(helloWorld)(name =>
      IO(s"Hello, $name!".asRight[Unit])
    )

  val docs = OpenAPIDocsInterpreter()
    .toOpenAPI(helloWorld, "Hello, World!", "1.0")
    .toYaml
  val swaggerUIRoute =
    Http4sServerInterpreter[IO]().toRoutes(SwaggerUI[IO](docs))

  implicit val runtime: IORuntime = IORuntime.global

  BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.Implicits.global)
    .bindHttp(8080, "localhost")
    .withHttpApp(
      Router("/" -> (helloWorldRoutes <+> swaggerUIRoute)).orNotFound
    )
    .resource
    .use(_ => IO.never)
    .as(ExitCode.Success)
    .unsafeRunSync()
