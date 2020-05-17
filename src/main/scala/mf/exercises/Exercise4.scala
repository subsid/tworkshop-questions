package mf.exercises

import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import mf.models._
import mf.http.Http

import scala.util.control.NonFatal

/**
  * Should ignore network failures
  * Should ignore parsing failures
  * @param ec
  */
class Exercise4(implicit ec: ExecutionContext) {
  def requestBatch(requests: List[Request], batchSize: Int): Future[List[ParsedResponse]] = {
    val batches = requests.grouped(batchSize).toList.map(BatchRequest.apply)
    batches.traverse(sendAndParseResponse).map(_.combineAll)
  }

  def sendAndParseResponse(batchRequest: BatchRequest): Future[List[ParsedResponse]] = {
    Http.getBatch(batchRequest).map(_.responses.flatMap(parseOrIgnore)).recoverWith({
      case NonFatal(_) => Future.successful(List())
    })
  }

  def parseOrIgnore(response: RawResponse): Option[ParsedResponse] = {
    ParsedResponse.parser(response.value).toOption
  }
}
