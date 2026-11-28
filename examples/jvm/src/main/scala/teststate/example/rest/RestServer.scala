package teststate.example.rest

import cats.effect.IO
import com.comcast.ip4s._
import org.http4s.ember.server.EmberServerBuilder
import scala.concurrent.duration._
import sttp.tapir.server.http4s.Http4sServerInterpreter

object RestServer {

  private val routes =
    Http4sServerInterpreter[IO]().toRoutes(ServerEndpoints.all)

  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"127.0.0.1")
    .withPort(port"0") // 0 here means choose a free port
    .withHttpApp(routes.orNotFound)
    .withShutdownTimeout(1.second)
    .build
}
