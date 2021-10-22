package vans

import io.circe.generic.semiauto.*
import io.circe.{Encoder, Json}
import io.circe.syntax.*

object Test10:
  case class Person1(name: String, age: Int)
  case class Person2(name: String, age: Int) derives Encoder.AsObject

  def encodePerson(p: Person1): Json =
    Json.fromFields(
      List(
        "name" -> Json.fromString(p.name),
        "age" -> Json.fromInt(p.age)
      )
    )

  def testManual = println(encodePerson(Person1("John", 74)).spaces2)

  def testSemiauto = println(deriveEncoder[Person1](Person1("John", 74)))

  def testGiven =
    given Encoder[Person1] = deriveEncoder[Person1]
    println(Person1("John", 74).asJson)

  def testDerives =
    println(Person2("John", 74).asJson)

@main def test10main() =
  import Test10.*
  println("Hello, world!")
  testDerives
