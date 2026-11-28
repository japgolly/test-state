package teststate.example.rest

import cats.effect.IO
import java.util.UUID
import sttp.client4.httpclient.cats.HttpClientCatsBackend
import sttp.model.Uri
import utest._
import MyTestState._
import TestDsl._

object RestTest extends TestSuite {
  override def tests = Tests {
    import cats.effect.unsafe.implicits.global
    startServerAndRunTest.unsafeRunSync()
  }

  def startServerAndRunTest: IO[Unit] =
    (for {
      server <- RestServer.server
      http   <- HttpClientCatsBackend.resource[IO]()
      } yield (server, http)
    ).use { case (server, http) =>
      for {
        id      <- IO.delay(UUID.randomUUID())
        baseUrl <- IO.delay(Uri.unsafeApply("127.0.0.1", server.address.getPort))
        ref     <- IO.pure(Ref(id, baseUrl, http))
        report  <- test(ref)
      } yield report.assert(useFailSettingsOnPass = true)
    }

  def test(ref: Ref): IO[Report[String]] = {
    val observer = dsl.observer(ref =>
      for {
        getUser <- ref.call(Endpoints.userGet)(ref.testUserId)
      } yield Obs(getUser.body.toOption)
    )

    val invariants =
      userName.assert.equal

    val test =
      createUser("Alice") >> deleteUser

    Plan(test, invariants)
      .withInitialState(State(None))
      .test(observer)
      .withRef(ref)
      .run()
  }
}
