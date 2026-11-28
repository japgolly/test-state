package teststate.example.rest

import cats.effect.IO
import sttp.tapir.server.ServerEndpoint

object ServerEndpoints {

  private val userGet = Endpoints.userGet
    .serverLogic[IO](id => Database.global.get(id).map(_.toRight(s"User not found: $id")))

  private val userPost = Endpoints.userPost
    .serverLogic[IO] { user =>
      Database.global.post(user) map {
        case true  => Right((s"/users/${user.id}", user))
        case false => Left(s"User already exists: ${user.id}")
      }
    }

  private val userDelete = Endpoints.userDelete
    .serverLogic[IO] { id =>
      Database.global.delete(id) map {
        case Some(user) => Right(user)
        case None       => Left(s"User not found: $id")
      }
    }

  val all = List[ServerEndpoint[Any, IO]](
    userGet,
    userPost,
    userDelete,
  )
}
