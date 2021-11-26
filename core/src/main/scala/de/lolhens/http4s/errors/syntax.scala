package de.lolhens.http4s.errors

import cats.MonadThrow
import cats.data.{EitherT, OptionT}
import cats.effect.Sync
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.http4s.{Response, Status}

import java.io.{PrintWriter, StringWriter}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

object syntax {

  implicit class ThrowableOps(val throwable: Throwable) extends AnyVal {
    def stackTraceString: String = {
      val stringWriter = new StringWriter()
      throwable.printStackTrace(new PrintWriter(stringWriter))
      stringWriter.toString
    }
  }

  implicit class EitherTResponseOps[F[_], A](val eitherT: EitherT[F, Response[F], A]) extends AnyVal {
    def throwErrorResponse(implicit F: MonadThrow[F]): F[A] =
      eitherT.leftSemiflatMap[A](response => F.raiseError(ErrorResponse[F](response))).merge
  }

  implicit def eitherTResponseNothingOps[F[_]](eitherT: EitherT[F, Response[F], Nothing]): EitherTResponseOps[F, Nothing] =
    new EitherTResponseOps[F, Nothing](eitherT)

  implicit class ResponseOps[F[_], A](val f: F[A]) extends AnyVal {
    def attemptEitherT(implicit F: MonadThrow[F]): EitherT[F, Throwable, A] =
      EitherT(f.attempt)

    def orStackTraceString(implicit F: Sync[F]): EitherT[F, String, A] = {
      val ResponseF = implicitly[ClassTag[Response[F]]]
      f.attemptEitherT.leftSemiflatMap {
        case e@ErrorResponse(ResponseF(_)) =>
          F.raiseError(e)

        case NonFatal(throwable) =>
          F.delay(throwable.stackTraceString)

        case throwable =>
          F.raiseError(throwable)
      }
    }

    def orErrorResponse(status: Status)
                       (implicit
                        throwableResponseLogger: ErrorResponseLogger[Throwable],
                        throwableResponseEncoder: ErrorResponseEncoder[Throwable],
                        F: Sync[F],
                       ): EitherT[F, Response[F], A] = {
      val ResponseF = implicitly[ClassTag[Response[F]]]
      f.attemptEitherT.leftSemiflatMap {
        case ErrorResponse(ResponseF(response)) =>
          F.delay(response)

        case NonFatal(throwable) =>
          F.defer {
            for {
              _ <- throwableResponseLogger.log(throwable)
              response <- throwableResponseEncoder.response(status, throwable)
            } yield
              response
          }

        case throwable =>
          F.raiseError(throwable)
      }
    }
  }

  implicit def responseNothingOps[F[_]](f: F[Nothing]): ResponseOps[F, Nothing] =
    new ResponseOps[F, Nothing](f)

  implicit class EitherOps[E, A](val either: Either[E, A]) extends AnyVal {
    def toErrorResponse[F[_]](status: Status)
                             (implicit
                              errorResponseLogger: ErrorResponseLogger[E],
                              errorResponseEncoder: ErrorResponseEncoder[E],
                              F: Sync[F],
                             ): EitherT[F, Response[F], A] =
      either match {
        case Left(error) =>
          EitherT.left(F.defer {
            for {
              _ <- errorResponseLogger.log(error)
              response <- errorResponseEncoder.response(status, error)
            } yield
              response
          })

        case Right(value) =>
          EitherT.rightT(value)
      }
  }

  implicit class OptionOps[A](val option: Option[A]) extends AnyVal {
    def toErrorResponse[F[_]](status: Status)
                             (implicit
                              errorResponseLogger: ErrorResponseLogger[Unit],
                              errorResponseEncoder: ErrorResponseEncoder[Unit],
                              F: Sync[F],
                             ): EitherT[F, Response[F], A] =
      option match {
        case None =>
          EitherT.left(F.defer {
            for {
              _ <- errorResponseLogger.log(())
              response <- errorResponseEncoder.response(status, ())
            } yield
              response
          })

        case Some(value) =>
          EitherT.rightT(value)
      }
  }

  implicit class EitherTOps[F[_], E, A](val eitherT: EitherT[F, E, A]) extends AnyVal {
    def toErrorResponse(status: Status)
                       (implicit
                        errorResponseLogger: ErrorResponseLogger[E],
                        errorResponseEncoder: ErrorResponseEncoder[E],
                        throwableResponseLogger: ErrorResponseLogger[Throwable],
                        throwableResponseEncoder: ErrorResponseEncoder[Throwable],
                        F: Sync[F],
                       ): EitherT[F, Response[F], A] =
      eitherT.value.orErrorResponse(status).flatMap(_.toErrorResponse(status))
  }

  implicit class OptionTOps[F[_], A](val optionT: OptionT[F, A]) extends AnyVal {
    def toErrorResponse(status: Status)
                       (implicit
                        errorResponseLogger: ErrorResponseLogger[Unit],
                        errorResponseEncoder: ErrorResponseEncoder[Unit],
                        throwableResponseLogger: ErrorResponseLogger[Throwable],
                        throwableResponseEncoder: ErrorResponseEncoder[Throwable],
                        F: Sync[F],
                       ): EitherT[F, Response[F], A] =
      optionT.value.orErrorResponse(status).flatMap(_.toErrorResponse(status))
  }

  implicit def eitherTNothingOps[F[_], E](eitherT: EitherT[F, E, Nothing]): EitherTOps[F, E, Nothing] =
    new EitherTOps[F, E, Nothing](eitherT)

}
