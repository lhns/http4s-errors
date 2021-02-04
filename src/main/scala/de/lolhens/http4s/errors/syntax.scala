package de.lolhens.http4s.errors

import cats.data.EitherT
import cats.effect.{BracketThrow, Sync}
import cats.syntax.applicativeError._
import org.http4s.Response
import org.http4s.dsl.impl.EntityResponseGenerator

import java.io.{PrintWriter, StringWriter}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

object syntax {

  implicit class ThrowableOps(val throwable: Throwable) extends AnyVal {
    def stackTrace: String = {
      val stringWriter = new StringWriter()
      throwable.printStackTrace(new PrintWriter(stringWriter))
      stringWriter.toString
    }
  }

  implicit class EitherTResponseOps[F[_], A](val eitherT: EitherT[F, Response[F], A]) extends AnyVal {
    def orError(implicit BracketThrowF: BracketThrow[F]): F[A] =
      eitherT.leftSemiflatMap[A](response => BracketThrowF.raiseError(ResponseException[F](response))).merge
  }

  implicit def eitherTResponseNothingOps[F[_]](eitherT: EitherT[F, Response[F], Nothing]): EitherTResponseOps[F, Nothing] =
    new EitherTResponseOps[F, Nothing](eitherT)

  implicit class ResponseOps[F[_], A](val f: F[A]) extends AnyVal {
    private val ResponseF = implicitly[ClassTag[Response[F]]]

    def orStacktrace(implicit SyncF: Sync[F]): EitherT[F, String, A] = {
      EitherT(f.attempt).leftSemiflatMap {
        case e@ResponseException(ResponseF(_)) =>
          SyncF.raiseError(e)

        case NonFatal(throwable) =>
          SyncF.delay(throwable.stackTrace)

        case throwable =>
          SyncF.raiseError(throwable)
      }
    }

    def orErrorResponse(status: EntityResponseGenerator[F, F])
                       (implicit
                        errorResponseLogger: ErrorResponseLogger[Throwable],
                        errorResponseEncoder: ErrorResponseEncoder[Throwable],
                        SyncF: Sync[F],
                       ): EitherT[F, Response[F], A] = {
      EitherT(f.attempt).leftSemiflatMap {
        case ResponseException(response@ResponseF(_)) =>
          SyncF.delay(response)

        case NonFatal(throwable) =>
          SyncF.defer {
            for {
              _ <- errorResponseLogger.log(throwable)
              response <- status(errorResponseEncoder.response(throwable))
            } yield
              response
          }

        case throwable =>
          SyncF.raiseError(throwable)
      }
    }
  }

  /*implicit class EitherTOps[F[_], E, A](val eitherT: EitherT[F, E, A]) extends AnyVal {
    def orErrorResponse(status: EntityResponseGenerator[F, F])
                       (implicit
                        throwableResponseLogger: ErrorResponseLogger[Throwable],
                        throwableResponseEncoder: ErrorResponseEncoder[Throwable],
                        errorResponseLogger: ErrorResponseLogger[E],
                        errorResponseEncoder: ErrorResponseEncoder[E],
                        SyncF: Sync[F],
                       ): EitherT[F, Response[F], A]
  }*/

}
