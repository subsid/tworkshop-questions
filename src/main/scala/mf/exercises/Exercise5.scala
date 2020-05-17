package mf.exercises

import cats.implicits._
import cats.data.ValidatedNel

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import mf.models._
import mf.http.Http

import scala.util.control.NonFatal

/**
  * Should not stop for any Http.getBatch failures
  * Should fail if any BatchResponse.response element fails to parse
  * Should collect all parsing failures and all network failures
  */
class Exercise5(implicit ec: ExecutionContext) {
  def requestBatch(requests: List[Request], batchSize: Int): Future[(List[ServiceError], List[ParsedResponse])] = {
    val batches = requests.grouped(batchSize).toList.map(BatchRequest.apply)
    batches.traverse(sendAndParseResponse).map(_.combineAll)
  }

  def sendAndParseResponse(request: BatchRequest): Future[(List[ServiceError], List[ParsedResponse])] = {
    Http.getBatch(request).map(parseResponses).recoverWith({
      case NonFatal(e) => Future.successful((List(ServiceError(e.getMessage)), List()))
    })
  }

  def parseResponses(batchResponse: BatchResponse): (List[ServiceError], List[ParsedResponse]) = {
    batchResponse.responses.foldMap(
      rr => ParsedResponse.parser(rr.value).fold(
        t => (List(ServiceError(t.getMessage)), Nil),
        res => (Nil, List(res))
      )
    )
  }
}

