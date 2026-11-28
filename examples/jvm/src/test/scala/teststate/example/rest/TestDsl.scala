package teststate.example.rest

import cats.effect.IO
import java.time.Instant
import java.util.UUID
import sttp.client4.{Response, WebSocketBackend}
import sttp.model.Uri
import sttp.tapir.PublicEndpoint
import sttp.tapir.client.sttp4.SttpClientInterpreter
import teststate.example.rest.TestState._

object TestDsl {

  case class Ref(testUserId: UUID, baseUri: Uri, http: WebSocketBackend[IO]) {
    def call[I, E, O](endpoint: PublicEndpoint[I, E, O, Any])(input: I): IO[Response[Either[E, O]]] =
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(endpoint, Some(baseUri))
        .apply(input)
        .send(http)
  }

  case class Obs(user: Option[User])

  case class State(userName: Option[String])

  val dsl = Dsl.io[Ref, Obs, State]

  val userName = dsl.focus("User name").obsAndState(_.user.map(_.name), _.userName)

  def createUser(name: String) = dsl.action(s"Create test user: $name").attempt { ros =>
    for {
      now  <- IO.delay(Instant.now())
      user  = User(ros.ref.testUserId, name, now)
      resp <- ros.ref.call(Endpoints.userPost)(user)
    } yield resp.body.left.toOption
  }.updateState(_.copy(userName = Some(name)))

  def deleteUser = dsl.action("Delete test user").attempt { ros =>
    for {
      resp <- ros.ref.call(Endpoints.userDelete)(ros.ref.testUserId)
    } yield resp.body.left.toOption
  }.updateState(_.copy(userName = None))
}
