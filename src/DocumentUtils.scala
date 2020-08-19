import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object DocumentUtils {
  // Returns all the bigrams in this document
  def documentToBigrams(listLines: ListBuffer[String]): ListBuffer[(String, String)] = {
    @scala.annotation.tailrec
    def documentToBigramsBuffer(listLines: List[String], retValue: List[(String, String)]): List[(String, String)] = {
      // TODO Manage the first word
      listLines match {
        // At least two words left
        case head1::head2::tail => documentToBigramsBuffer(head2::tail, (head1, head2)::retValue)
        // At least one word left (last word, bigram (lastWord, ""))
        case head::tail => documentToBigramsBuffer(tail, (head, "")::retValue)
        // Nothing left
        case Nil => retValue
      }
    }
    // Reverse the list and add the first bigram ("", firstWord)
    val retList: List[(String, String)] = ("", listLines.head)::documentToBigramsBuffer(listLines.toList, List()).reverse
    retList.to(collection.mutable.ListBuffer)
  }

  // Returns all the trigrams in this document
  def documentToTrigrams(listLines: ListBuffer[String]): ListBuffer[(String, String, String)] = {
    @scala.annotation.tailrec
    def documentToTrigramsBuffer(listLines: List[String], retValue: List[(String, String, String)]): List[(String, String, String)] = {
      // TODO Manage the first word
      listLines match {
        // At least three words left
        case head1::head2::head3::tail => documentToTrigramsBuffer(head2::tail, (head1, head2, head3)::retValue)
        // At least two words left (previous last word, trigram (previousLastWord, lastWord, ""))
        case head1::head2::tail => documentToTrigramsBuffer(head2::tail, (head1, head2, "")::retValue)
        // At least one word left (last word, trigram (lastWord, "", ""))
        case head::tail => documentToTrigramsBuffer(tail, (head, "", "")::retValue)
        // Nothing left
        case Nil => retValue
      }
    }
    // Reverse the list and add the first and second trigrams ("", "", firstWord) & ("", firstWord, secondWord)
    val retList: List[(String, String, String)] =
      ("", "", listLines.head)::("", listLines.head, listLines.tail.head)::documentToTrigramsBuffer(listLines.toList, List()).reverse
    retList.to(collection.mutable.ListBuffer)
  }

  // Get the list of all the distinct objects in a document
  def getListDistinctObjectsFromDocument[A](listObjects: ListBuffer[A]): ListBuffer[A] =
    listObjects.distinct

  // Get the number of occurrences of each object in the document
  def getNumberIterationsEachObject[A](listObjects: ListBuffer[A]): Map[A, Int] = {
//    var startTime = System.nanoTime

    val ret = listObjects.foldLeft(Map[A,Int]() withDefaultValue 0) {
      (m,x) => m + (x -> (1 + m(x)))
    }

//    var duration = (System.nanoTime - startTime) / 1e9d
//    var seconds = FileUtils.compressedDouble(duration % 60)
//    var minutes = FileUtils.compressedDouble(BigDecimal(duration / 60).setScale(0, BigDecimal.RoundingMode.DOWN).toInt)
//    println(s"Execution of getNumberIterationsEachObject took $minutes minute(s) and $seconds seconds.")
//    startTime = System.nanoTime

//    val ret = listDistinctObjects.map(obj => (obj, listObjects.count(_ == obj))).toMap.withDefaultValue(0)
//    duration = (System.nanoTime - startTime) / 1e9d
//    seconds = FileUtils.compressedDouble(duration % 60)
//    minutes = FileUtils.compressedDouble(BigDecimal(duration / 60).setScale(0, BigDecimal.RoundingMode.DOWN).toInt)
//    println(s"Execution of getNumberIterationsEachObject took $minutes minute(s) and $seconds seconds.")
//    startTime = System.nanoTime
   ret
  }
}
