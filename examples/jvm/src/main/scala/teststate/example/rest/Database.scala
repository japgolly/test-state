package teststate.example.rest

import cats.effect.IO
import java.util.UUID

final class Database {

  private val lock = new AnyRef
  private var users = Map.empty[UUID, User]

  private def act[A](a: => A): IO[A] =
    IO.delay(lock.synchronized(a))

  def get(id: UUID): IO[Option[User]] = act {
    users.get(id)
  }

  def post(user: User): IO[Boolean] = act {
    if (users.contains(user.id))
      false
    else {
      users = users.updated(user.id, user)
      true
    }
  }

  def delete(id: UUID): IO[Option[User]] = act {
    val result = users.get(id)
    if (result.isDefined)
      users -= id
    result
  }
}

object Database {
  val global = new Database
}
