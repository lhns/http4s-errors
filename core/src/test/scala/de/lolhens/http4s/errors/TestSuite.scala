package de.lolhens.http4s.errors

import cats.effect.IO
import de.lolhens.http4s.errors.ErrorResponseEncoder.stacktrace._
import de.lolhens.http4s.errors.ErrorResponseLogger.stderr._
import de.lolhens.http4s.errors.syntax._
import org.http4s.Status
import org.http4s.dsl.io._

class TestSuite extends IOSuite {
  test("return error response") {
    (for {
      response <- IO("response").orErrorResponse(BadRequest)
      _ <- IO(throw new RuntimeException("expected error"))
        .orErrorResponse(InternalServerError).throwErrorResponse
        .orErrorResponse(BadRequest)
      response <- Ok(response).orErrorResponse(BadRequest)
    } yield
      response)
      .toErrorResponse(InternalServerError)
      .merge
      .flatMap(e => e.as[String].map((e, _)))
      .map {
        case (response, string) =>
          assertEquals(response.status.code, InternalServerError.code)
          assert(clue(string).contains("expected error"))
      }
  }

  test("ErrorResponseEncoder.instance") {
    val testMessage = "test"
    ErrorResponseEncoder.string.statusErrorResponseEncoderString.response[IO](
      Status.InternalServerError,
      testMessage
    ).flatMap(_.as[String]).map(string =>
      assertEquals(string, testMessage)
    )
  }
}
