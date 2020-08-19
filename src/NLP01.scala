import scala.collection.mutable.ListBuffer
import scala.util.Random

object NLP01 {
  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime

    val rand = new Random();

    def iterationChar(iter: Int, messUp: Double): Unit = {
      val iterationStartTime = System.nanoTime

      // Read the documents
      //val cleanListLinesEN: ListBuffer[String] = FileUtils.getListLines("data/TEXTEN1.txt")
      val cleanListLinesCZ: ListBuffer[String] = FileUtils.getListLines("data/TEXTCZ1.txt")

      // Mess up the characters here.
      //val listCharactersEN: ListBuffer[Char] = cleanListLinesEN.mkString("").toCharArray.distinct.to(collection.mutable.ListBuffer)
      val listCharactersCZ: ListBuffer[Char] = cleanListLinesCZ.mkString("").toCharArray.distinct.to(collection.mutable.ListBuffer)

      def getRandomObject[A](list: ListBuffer[A]): A =
        list(rand.nextInt(list.length))

      def messUpTextChar(cleanListLines: ListBuffer[String], listCharacters: ListBuffer[Char]): ListBuffer[String] = {
        // For each character in the text, mess it up if the value obtained from rand.nextFloat() yields a value inferior to "messUp"
        // rand.nextFloat() always returns values between 0.0 and 1.0.
        cleanListLines.map(
          word => word.map(
            char => {
              // Mess up
              if(rand.nextDouble < messUp) getRandomObject(listCharacters)
              // Do not mess up
              else char
            }
          )
        )
      }

      def messUpTextWord(cleanListLines: ListBuffer[String], cleanListWords: ListBuffer[String]): ListBuffer[String] = {
        // For each word in the text, mess it up if the value obtained from rand.nextFloat() yields a value inferior to "messUp"
        // rand.nextFloat() always returns values between 0.0 and 1.0.
        cleanListLines.map(
          word => {
              // Mess up
              if(rand.nextDouble < messUp) getRandomObject(cleanListWords)
              // Do not mess up
              else word
            }
        )
      }

      // Lists of clean distinct words (i)
      //val cleanListWordsEN: ListBuffer[String] = DocumentUtils.getListDistinctObjectsFromDocument(cleanListLinesEN)
      val cleanListWordsCZ: ListBuffer[String] = DocumentUtils.getListDistinctObjectsFromDocument(cleanListLinesCZ)

      //val listLinesEN: ListBuffer[String] = messUpTextChar(cleanListLinesEN, listCharactersEN)
      //val listLinesCZ: ListBuffer[String] = messUpTextChar(cleanListLinesCZ, listCharactersCZ)
      //val listLinesEN: ListBuffer[String] = messUpTextWord(cleanListLinesEN, cleanListWordsEN)
      val listLinesCZ: ListBuffer[String] = messUpTextWord(cleanListLinesCZ, cleanListWordsCZ)

      // Lists of bigrams
      //val listBigramsEN: ListBuffer[(String, String)] = DocumentUtils.documentToBigrams(listLinesEN)
      val listBigramsCZ: ListBuffer[(String, String)] = DocumentUtils.documentToBigrams(listLinesCZ)

      // Lists of distinct bigrams (i,j)
      //val listDistinctBigramsEN: ListBuffer[(String, String)] = DocumentUtils.getListDistinctObjectsFromDocument(listBigramsEN)
      val listDistinctBigramsCZ: ListBuffer[(String, String)] = DocumentUtils.getListDistinctObjectsFromDocument(listBigramsCZ)

      // Number of occurrences of each word in their respective documents Map(i => Int) = Map((i, _) => Int)
      // Consider the element "", which is at the beginning and the end of the document
      //val mapOccurrencesWordsEN: Map[String, Int] = DocumentUtils.getNumberIterationsEachObject(listLinesEN) + ("" -> 2)
      val mapOccurrencesWordsCZ: Map[String, Int] = DocumentUtils.getNumberIterationsEachObject(listLinesCZ) + ("" -> 2)

      // Number of occurrences of each bigram in their respective documents Map((i, j) => Int)
      //val mapOccurrencesBigramsEN: Map[(String, String), Int] = DocumentUtils.getNumberIterationsEachObject(listBigramsEN)
      val mapOccurrencesBigramsCZ: Map[(String, String), Int] = DocumentUtils.getNumberIterationsEachObject(listBigramsCZ)

      //val conditionalEntropyEN: Double = NLPUtils.conditionalEntropy(listDistinctBigramsEN, listBigramsEN.length, mapOccurrencesBigramsEN, mapOccurrencesWordsEN)
      val conditionalEntropyCZ: Double = NLPUtils.conditionalEntropy(listDistinctBigramsCZ, listBigramsCZ.length, mapOccurrencesBigramsCZ, mapOccurrencesWordsCZ)

      //println(s"Conditional entropy of the English text: $conditionalEntropyEN")
      println(s"Conditional entropy of the Czech text: $conditionalEntropyCZ")

      //println(s"Perplexity of the English text: ${NLPUtils.perplexity(conditionalEntropyEN)}")
      println(s"Perplexity of the Czech text: ${NLPUtils.perplexity(conditionalEntropyCZ)}")

      val iterationDuration = (System.nanoTime - iterationStartTime) / 1e9d
      val iterationSeconds = FileUtils.compressedDouble(iterationDuration % 60)
      val iterationMinutes = FileUtils.compressedDouble(BigDecimal(iterationDuration / 60).setScale(0, BigDecimal.RoundingMode.DOWN).toInt)
      val duration = (System.nanoTime - startTime) / 1e9d
      val seconds = FileUtils.compressedDouble(duration % 60)
      val minutes = FileUtils.compressedDouble(BigDecimal(duration / 60).setScale(0, BigDecimal.RoundingMode.DOWN).toInt)
      println(s"Execution of the iteration n°$iter with mess-up ${messUp * 100}% took $iterationMinutes minute(s) and $iterationSeconds seconds. The program has been running for $minutes minute(s) and $seconds seconds.")
    }

    def information(): Unit = {
      // Read the documents
      val listLinesEN: ListBuffer[String] = FileUtils.getListLines("data/TEXTEN1.txt")
      val listLinesCZ: ListBuffer[String] = FileUtils.getListLines("data/TEXTCZ1.txt")

      // Lists of distinct words
      val listDistinctWordsEN: ListBuffer[String] = DocumentUtils.getListDistinctObjectsFromDocument(listLinesEN)
      val listDistinctWordsCZ: ListBuffer[String] = DocumentUtils.getListDistinctObjectsFromDocument(listLinesCZ)

      // Word count
      println(s"Number of English words: ${listLinesEN.length} / distinct words: ${listDistinctWordsEN.length}")
      println(s"Number of Czech words: ${listLinesCZ.length} / distinct words: ${listDistinctWordsCZ.length}")

      // Number of characters
      println(s"Number of English characters: ${listLinesEN.map(str => str.length).sum}")
      println(s"Number of Czech characters: ${listLinesCZ.map(str => str.length).sum}")

      // Frequency of the most frequent words
      // Lists of distinct words (i)
      val listWordsEN: ListBuffer[String] = DocumentUtils.getListDistinctObjectsFromDocument(listLinesEN)
      val listWordsCZ: ListBuffer[String] = DocumentUtils.getListDistinctObjectsFromDocument(listLinesCZ)

      val sortedWordsWithFrequencyEN: ListBuffer[(String, Int)] = DocumentUtils.getNumberIterationsEachObject(listLinesEN).to(collection.mutable.ListBuffer).sortBy(_._2).reverse
      val sortedWordsWithFrequencyCZ: ListBuffer[(String, Int)] = DocumentUtils.getNumberIterationsEachObject(listLinesCZ).to(collection.mutable.ListBuffer).sortBy(_._2).reverse

      for(i <- 0 to 10) println(sortedWordsWithFrequencyEN(i))
      for(i <- 0 to 10) println(sortedWordsWithFrequencyCZ(i))

      // Number of words with frequency 1
      println(s"Number of words with frequency 1 in the English text: ${sortedWordsWithFrequencyEN.count(_._2 == 1)}")
      println(s"Number of words with frequency 1 in the Czech text: ${sortedWordsWithFrequencyCZ.count(_._2 == 1)}")

    }


    information()


    val messUps: ListBuffer[Double] = ListBuffer(0.0, 0.001/100, 0.01/100, 0.1/100, 1.0/100, 5.0/100, 10.0/100)
    for(messUp <- messUps)
      if(messUp == 0.0) iterationChar(0, messUp)
      else for(i <- 1 to 10) iterationChar(i, messUp)

    val duration = (System.nanoTime - startTime) / 1e9d
    val seconds = FileUtils.compressedDouble(duration % 60)
    val minutes = FileUtils.compressedDouble(BigDecimal(duration / 60).setScale(0, BigDecimal.RoundingMode.DOWN).toInt)
    println(s"Execution of the program took $minutes minute(s) and $seconds seconds.")
  }
}
