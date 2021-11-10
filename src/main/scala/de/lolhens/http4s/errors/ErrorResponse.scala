package de.lolhens.http4s.errors

import org.http4s.Response

import scala.util.control.NoStackTrace

case class ErrorResponse[F[_]](response: Response[F]) extends Throwable with NoStackTrace
