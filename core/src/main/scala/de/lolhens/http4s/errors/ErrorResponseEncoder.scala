package de.lolhens.http4s.errors

import cats.Id
import cats.effect.Sync
import de.lolhens.http4s.errors.syntax._
import org.http4s.{Response, Status}

trait ErrorResponseEncoder[-E] {
  def response[F[_] : Sync](status: Status, error: E): F[Response[F]]

  final def contramap[E2](f: E2 => E): ErrorResponseEncoder[E2] = new ErrorResponseEncoder[E2] {
    override def response[F[_] : Sync](status: Status, error: E2): F[Response[F]] = ErrorResponseEncoder.this.response(status, f(error))
  }
}

object ErrorResponseEncoder {
  def apply[E](implicit errorResponseEncoder: ErrorResponseEncoder[E]): ErrorResponseEncoder[E] = errorResponseEncoder

  def instance[E](f: (Status, E) => String): ErrorResponseEncoder[E] = new ErrorResponseEncoder[E] {
    override def response[F[_] : Sync](status: Status, error: E): F[Response[F]] =
      Sync[F].delay(Response[F](status).withEntity(f(status, error)))
  }

  private val _errorResponseEncoderResponse: ErrorResponseEncoder[Response[Id]] = new ErrorResponseEncoder[Response[Id]] {
    override def response[F[_] : Sync](status: Status, error: Response[Id]): F[Response[F]] =
      Sync[F].pure(error.asInstanceOf[Response[F]])
  }

  @inline implicit def errorResponseEncoderResponse[F[_]]: ErrorResponseEncoder[Response[F]] =
    _errorResponseEncoderResponse.asInstanceOf[ErrorResponseEncoder[Response[F]]]

  object empty {
    private val _emptyErrorResponseEncoder: ErrorResponseEncoder[Any] =
      instance((_, _) => "")

    @inline implicit def emptyErrorResponseEncoder[E]: ErrorResponseEncoder[E] = _emptyErrorResponseEncoder
  }

  object status {
    private val _statusErrorResponseEncoder: ErrorResponseEncoder[Any] =
      instance((status, _) => status.reason)

    @inline implicit def statusErrorResponseEncoder[E]: ErrorResponseEncoder[E] = _statusErrorResponseEncoder
  }

  object string {
    implicit val statusErrorResponseEncoderString: ErrorResponseEncoder[String] =
      instance((_, string) => string)
  }

  object message {
    implicit val messageErrorResponseEncoder: ErrorResponseEncoder[Throwable] =
      instance((_, throwable) => throwable.getMessage)
  }

  object stacktrace {
    implicit val stacktraceErrorResponseEncoderThrowable: ErrorResponseEncoder[Throwable] =
      instance((_, throwable) => throwable.stackTraceString)
  }

}
