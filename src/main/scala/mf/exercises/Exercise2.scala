package mf.exercises

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import mf.models._
import cats.implicits._
import cats.syntax.option._
import mf.http.Http

// Should fail if any call to Http.getBatch fails
// Should ignore (drop) parsing failures if the ParsedResponse.parser fails to parse an element
class Exercise2(implicit ec: ExecutionContext) {
  def requestBatch(requests: List[Request], batchSize: Int): Future[List[ParsedResponse]] = {
    val batches: List[BatchRequest] = requests.grouped(batchSize).toList.map(BatchRequest.apply)

    batches.traverse(sendAndParse).map(_.combineAll)  // List[Future[List[ParsedResponse]]
  }

  def sendAndParse(batchRequest: BatchRequest): Future[List[ParsedResponse]] = {
    // Future[List[RawResponse]] => Future[List[ParsedResponse]]
    Http.getBatch(batchRequest).map(parseResponse)
  }

  def parseResponse(batchResponse: BatchResponse): List[ParsedResponse] = {
    batchResponse.responses.flatMap((response) => {
      ParsedResponse.parser(response.value).toOption
    })
  }
}

