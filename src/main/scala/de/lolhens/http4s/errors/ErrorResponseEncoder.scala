package de.lolhens.http4s.errors

import cats.effect.Sync
import de.lolhens.http4s.errors.syntax._
import org.http4s.{Response, Status}

trait ErrorResponseEncoder[-E] {
  def response[F[_] : Sync](status: Status, error: E): F[Response[F]]
}

object ErrorResponseEncoder {

  object status {
    implicit val statusErrorResponseEncoder: ErrorResponseEncoder[Throwable] = new ErrorResponseEncoder[Throwable] {
      override def response[F[_] : Sync](status: Status, throwable: Throwable): F[Response[F]] =
        Sync[F].delay(Response[F](status).withEntity(status.reason))
    }
  }

  object stacktrace {
    implicit val stacktraceErrorResponseEncoder: ErrorResponseEncoder[Throwable] = new ErrorResponseEncoder[Throwable] {
      override def response[F[_] : Sync](status: Status, throwable: Throwable): F[Response[F]] =
        Sync[F].delay(Response[F](status).withEntity(throwable.stackTraceString))
    }
  }

}
