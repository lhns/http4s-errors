package de.lolhens.http4s.errors

import cats.effect.Sync
import de.lolhens.http4s.errors.syntax._
import org.http4s.{Response, Status}

trait ErrorResponseEncoder[-E] {
  def response[F[_] : Sync](status: Status, error: E): F[Response[F]]
}

object ErrorResponseEncoder {
  def apply[E](implicit errorResponseEncoder: ErrorResponseEncoder[E]): ErrorResponseEncoder[E] = errorResponseEncoder

  def instance[E](f: (Status, E) => String): ErrorResponseEncoder[E] = new ErrorResponseEncoder[E] {
    override def response[F[_] : Sync](status: Status, error: E): F[Response[F]] =
      Sync[F].delay(Response[F](status).withEntity(f(status, error)))
  }

  object status {
    implicit val statusErrorResponseEncoder: ErrorResponseEncoder[Throwable] =
      instance((status, _) => status.reason)
  }

  object message {
    implicit val messageErrorResponseEncoder: ErrorResponseEncoder[Throwable] =
      instance((_, throwable) => throwable.getMessage)
  }

  object stacktrace {
    implicit val stacktraceErrorResponseEncoder: ErrorResponseEncoder[Throwable] =
      instance((_, throwable) => throwable.stackTraceString)
  }

}
