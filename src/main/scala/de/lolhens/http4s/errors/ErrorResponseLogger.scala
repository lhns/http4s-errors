package de.lolhens.http4s.errors

import cats.effect.Sync
import de.lolhens.http4s.errors.syntax._

trait ErrorResponseLogger[-E] {
  def log[F[_] : Sync](error: E): F[Unit]
}

object ErrorResponseLogger {

  object stderr {
    implicit val stacktraceErrorResponseLogger: ErrorResponseLogger[Throwable] = new ErrorResponseLogger[Throwable] {
      override def log[F[_] : Sync](throwable: Throwable): F[Unit] =
        Sync[F].delay(System.err.println(throwable.stackTrace))
    }
  }

}
