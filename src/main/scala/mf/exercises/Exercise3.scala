package mf.exercises

import cats.data.ValidatedNel
import cats.syntax.validated._
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import mf.models._
import mf.http.Http

/**
  * Should fail if any network call to Http.getbatch fails
  * Should fail if any BatchResponse.response element fails to parse
  * Should collect all parsing failures
  */
class Exercise3(implicit ec: ExecutionContext) {
  def requestBatch(requests: List[Request], batchSize: Int): Future[ValidatedNel[ServiceError, List[ParsedResponse]]] = {
    val batches = requests.grouped(batchSize).toList.map(BatchRequest.apply)
    batches.map(sendAndParseResponse).sequence.map(_.combineAll)
  }

  def sendAndParseResponse(batchRequest: BatchRequest): Future[ValidatedNel[ServiceError, List[ParsedResponse]]] = {
      Http.getBatch(batchRequest).map(_.responses.traverse(validateAndParse))
  }

  def validateAndParse(response: RawResponse): ValidatedNel[ServiceError, ParsedResponse] = {
    ParsedResponse.parser(response.value).toEither.leftMap(t => ServiceError(t.getMessage)).toValidatedNel
  }
}
