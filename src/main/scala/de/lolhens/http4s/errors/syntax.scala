package de.lolhens.http4s.errors

import cats.data.EitherT
import cats.effect.{BracketThrow, Sync}
import cats.syntax.applicativeError._
import cats.syntax.either._
import org.http4s.Response
import org.http4s.dsl.impl.EntityResponseGenerator

import java.io.{PrintWriter, StringWriter}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

object syntax {

  implicit class EitherTResponseOps[F[_], A](val eitherT: EitherT[F, Response[F], A]) extends AnyVal {
    def orError(implicit BracketThrowF: BracketThrow[F]): F[A] =
      eitherT.leftSemiflatMap[A](response => BracketThrowF.raiseError(ResponseException[F](response))).merge
  }

  implicit def eitherTResponseNothingOps[F[_]](eitherT: EitherT[F, Response[F], Nothing]): EitherTResponseOps[F, Nothing] =
    new EitherTResponseOps[F, Nothing](eitherT)

  implicit class ResponseOps[F[_], A](val f: F[A]) extends AnyVal {
    def orStacktrace(implicit BracketThrowF: BracketThrow[F]): EitherT[F, Either[Response[F], String], A] = {
      val ResponseF = implicitly[ClassTag[Response[F]]]

      EitherT(f.attempt).leftMap {
        case ResponseException(ResponseF(response)) =>
          Either.left(response)

        case NonFatal(throwable) =>
          val stringWriter = new StringWriter()
          throwable.printStackTrace(new PrintWriter(stringWriter))
          Either.right(stringWriter.toString)

        case throwable => throw throwable
      }
    }

    def orErrorResponse(status: EntityResponseGenerator[F, F],
                        log: String => Unit = System.err.println)
                       (implicit SyncF: Sync[F]): EitherT[F, Response[F], A] =
      f.orStacktrace.leftSemiflatMap(_.fold(SyncF.delay[Response[F]](_), { stacktrace =>
        log(stacktrace)
        status(stacktrace)
      }))
  }

}
