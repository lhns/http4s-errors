package de.lolhens.http4s.errors

import cats.effect.Sync
import org.http4s.Response

trait ErrorResponseEncoder[-E] {
  def response[F[_] : Sync](error: E): F[Response[F]]
}
