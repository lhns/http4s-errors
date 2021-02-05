package de.lolhens.http4s.errors

import org.http4s.Response

case class ErrorResponse[F[_]](response: Response[F]) extends Throwable
