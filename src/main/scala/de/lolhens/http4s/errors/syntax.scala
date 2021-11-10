package de.lolhens.http4s.errors

import cats.data.{EitherT, OptionT}
import cats.effect.{BracketThrow, Sync}
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
    def throwErrorResponse(implicit BracketThrowF: BracketThrow[F]): F[A] =
      eitherT.leftSemiflatMap[A](response => BracketThrowF.raiseError(ErrorResponse[F](response))).merge
  }

  implicit def eitherTResponseNothingOps[F[_]](eitherT: EitherT[F, Response[F], Nothing]): EitherTResponseOps[F, Nothing] =
    new EitherTResponseOps[F, Nothing](eitherT)

  implicit class ResponseOps[F[_], A](val f: F[A]) extends AnyVal {
    def attemptEitherT(implicit BracketThrowF: BracketThrow[F]): EitherT[F, Throwable, A] =
      EitherT(f.attempt)

    def orStackTraceString(implicit SyncF: Sync[F]): EitherT[F, String, A] = {
      val ResponseF = implicitly[ClassTag[Response[F]]]
      f.attemptEitherT.leftSemiflatMap {
        case e@ErrorResponse(ResponseF(_)) =>
          SyncF.raiseError(e)

        case NonFatal(throwable) =>
          SyncF.delay(throwable.stackTraceString)

        case throwable =>
          SyncF.raiseError(throwable)
      }
    }

    def orErrorResponse(status: Status)
                       (implicit
                        throwableResponseLogger: ErrorResponseLogger[Throwable],
                        throwableResponseEncoder: ErrorResponseEncoder[Throwable],
                        SyncF: Sync[F],
                       ): EitherT[F, Response[F], A] = {
      val ResponseF = implicitly[ClassTag[Response[F]]]
      f.attemptEitherT.leftSemiflatMap {
        case ErrorResponse(ResponseF(response)) =>
          SyncF.delay(response)

        case NonFatal(throwable) =>
          SyncF.defer {
            for {
              _ <- throwableResponseLogger.log(throwable)
              response <- throwableResponseEncoder.response(status, throwable)
            } yield
              response
          }

        case throwable =>
          SyncF.raiseError(throwable)
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
                              SyncF: Sync[F],
                             ): EitherT[F, Response[F], A] =
      either match {
        case Left(error) =>
          EitherT.left(SyncF.defer {
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
                              SyncF: Sync[F],
                             ): EitherT[F, Response[F], A] =
      option match {
        case None =>
          EitherT.left(SyncF.defer {
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
                        SyncF: Sync[F],
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
                        SyncF: Sync[F],
                       ): EitherT[F, Response[F], A] =
      optionT.value.orErrorResponse(status).flatMap(_.toErrorResponse(status))
  }

  implicit def eitherTNothingOps[F[_], E](eitherT: EitherT[F, E, Nothing]): EitherTOps[F, E, Nothing] =
    new EitherTOps[F, E, Nothing](eitherT)

}
