package de.lolhens.http4s.errors

import cats.effect.IO

import scala.concurrent.Future

abstract class IOSuite extends munit.TaglessFinalSuite[IO] {
  override protected def toFuture[A](f: IO[A]): Future[A] =
    f.unsafeToFuture()
}
