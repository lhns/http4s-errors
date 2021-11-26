package de.lolhens.http4s.errors

import cats.effect.Sync
import de.lolhens.http4s.errors.syntax._
import org.http4s.Response
import org.slf4j.Logger

trait ErrorResponseLogger[-E] {
  def log[F[_] : Sync](error: E): F[Unit]

  final def contramap[E2](f: E2 => E): ErrorResponseLogger[E2] = new ErrorResponseLogger[E2] {
    override def log[F[_] : Sync](error: E2): F[Unit] = ErrorResponseLogger.this.log(f(error))
  }
}

object ErrorResponseLogger {
  def apply[E](implicit errorResponseLogger: ErrorResponseLogger[E]): ErrorResponseLogger[E] = errorResponseLogger

  def instance[E](f: E => Unit): ErrorResponseLogger[E] = new ErrorResponseLogger[E] {
    override def log[F[_] : Sync](error: E): F[Unit] = Sync[F].delay(f(error))
  }

  @inline implicit def errorResponseLoggerResponse[F[_]]: ErrorResponseLogger[Response[F]] = noop.noopErrorResponseLogger

  def stringLogger(logger: Logger): ErrorResponseLogger[String] =
    instance(string => if (logger.isErrorEnabled) logger.error(string))

  @deprecated
  @inline def logger(logger: Logger): ErrorResponseLogger[Throwable] = throwableLogger(logger)

  def throwableLogger(logger: Logger): ErrorResponseLogger[Throwable] =
    instance(throwable => if (logger.isErrorEnabled) logger.error(throwable.getMessage, throwable))

  object noop {
    private val _noopErrorResponseLogger: ErrorResponseLogger[Any] = new ErrorResponseLogger[Any] {
      override def log[F[_] : Sync](error: Any): F[Unit] = Sync[F].unit
    }

    @inline implicit def noopErrorResponseLogger[E]: ErrorResponseLogger[E] = _noopErrorResponseLogger
  }

  object stderr {
    implicit val stdoutErrorResponseLoggerString: ErrorResponseLogger[String] =
      ErrorResponseLogger.instance(string => System.err.println(string))

    implicit val stdoutErrorResponseLoggerThrowable: ErrorResponseLogger[Throwable] =
      stdoutErrorResponseLoggerString.contramap[Throwable](_.stackTraceString)
  }

}
