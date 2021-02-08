package de.lolhens.http4s.errors

import cats.effect.Sync
import de.lolhens.http4s.errors.syntax._
import org.slf4j.Logger

trait ErrorResponseLogger[-E] {
  def log[F[_] : Sync](error: E): F[Unit]
}

object ErrorResponseLogger {
  def apply[E](implicit errorResponseLogger: ErrorResponseLogger[E]): ErrorResponseLogger[E] = errorResponseLogger

  def instance[E](f: E => Unit): ErrorResponseLogger[E] = new ErrorResponseLogger[E] {
    override def log[F[_] : Sync](error: E): F[Unit] = Sync[F].delay(f(error))
  }

  def logger(logger: Logger): ErrorResponseLogger[Throwable] =
    instance(throwable => if (logger.isErrorEnabled) logger.error(throwable.getMessage, throwable))

  object noop {
    implicit val noopErrorResponseLogger: ErrorResponseLogger[Throwable] = new ErrorResponseLogger[Throwable] {
      override def log[F[_] : Sync](throwable: Throwable): F[Unit] = Sync[F].unit
    }
  }

  object stderr {
    implicit val stdoutErrorResponseLogger: ErrorResponseLogger[Throwable] =
      ErrorResponseLogger.instance[Throwable](throwable => System.err.println(throwable.stackTraceString))
  }

}
