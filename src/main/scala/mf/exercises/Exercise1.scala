package mf.exercises

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import mf.models._
import mf.http.Http
import cats.data.Nested
import cats.implicits._

/**
  * Should fail if any call to Http.getBatch fails
  * Should fail if any BatchResponse.response element fails to parse into a ParsedResponse using the
  * ParsedResponse.parser
  */
class Exercise1(implicit ec: ExecutionContext) {
  def requestBatch(requests: List[Request], batchSize: Int): Future[Either[ServiceError, List[ParsedResponse]]] = {
    val batchRequests: List[BatchRequest] = requests.grouped(batchSize).toList.map(BatchRequest.apply)
    batchRequests
      .traverse(sendAndParse)
      .map(_.combineAll)
  }

  private def sendAndParse(batchRequest: BatchRequest): Future[Either[ServiceError, List[ParsedResponse]]] = {
    val batchResponse: Future[List[RawResponse]] = Http.getBatch(batchRequest).map(_.responses)

    batchResponse.map(
      _.traverse(rr => ParsedResponse.parser(rr.value).toEither.leftMap((e) => ServiceError(e.getMessage)))
    )
  }
}