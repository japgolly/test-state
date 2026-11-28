package teststate.example.rest

import java.util.UUID
import sttp.model.StatusCode
import sttp.tapir._
import sttp.tapir.json.circe._

object Endpoints {

  val userGet = endpoint
    .get
    .in("users" / path[UUID]("id"))
    .errorOut(statusCode(StatusCode.NotFound).and(stringBody))
    .out(jsonBody[User])

  val userPost = endpoint
    .post
    .in("users")
    .in(jsonBody[User])
    .errorOut(statusCode(StatusCode.Conflict).and(stringBody))
    .out(statusCode(StatusCode.Created))
    .out(header[String]("Location"))
    .out(jsonBody[User])

  val userDelete = endpoint
    .delete
    .in("users" / path[UUID]("id"))
    .errorOut(statusCode(StatusCode.NotFound).and(stringBody))
    .out(statusCode(StatusCode.Ok))
    .out(jsonBody[User])
}
