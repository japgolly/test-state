package teststate.example.rest

import java.util.UUID
import java.time.Instant
import io.circe.Codec
import sttp.tapir.Schema

final case class User(id: UUID, name: String, createdAt: Instant)

object User {
  implicit val codec: Codec.AsObject[User] =
    Codec.forProduct3("id", "name", "createdAt")(apply)(u => (u.id, u.name, u.createdAt))

  implicit val schema: Schema[User] =
    Schema.derived[User]
}
