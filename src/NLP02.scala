import scala.collection.mutable.ListBuffer

object NLP02 {
  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime

    // Read the documents (uncomment for English or Czech)
    //val cleanListLinesENCZ: ListBuffer[String] = FileUtils.getListLines("data/TEXTEN1.txt")
    val cleanListLinesENCZ: ListBuffer[String] = FileUtils.getListLines("data/TEXTCZ1.txt")

    // Cut the documents in three datasets
    // Lengths
    // Test data: 20,000
    val testDataLengthENCZ = 20000
    // Held-out data: 40,000
    val heldOutDataLength = 40000
    // Training data: everything else
    val trainingDataLengthENCZ = cleanListLinesENCZ.length - testDataLengthENCZ - heldOutDataLength

    // Actual cutting
    val trainingDataENCZ = cleanListLinesENCZ.slice(0, trainingDataLengthENCZ)
    val heldOutDataENCZ = cleanListLinesENCZ.slice(trainingDataLengthENCZ, trainingDataLengthENCZ + heldOutDataLength)
    val testDataENCZ = cleanListLinesENCZ.slice(trainingDataLengthENCZ + heldOutDataLength, cleanListLinesENCZ.length)

    // Useful lists
    // Lists of distinct words (i)
    val listDistinctWordsTrainingENCZ: ListBuffer[String] = DocumentUtils.getListDistinctObjectsFromDocument(trainingDataENCZ)

    // Lists of bigrams (i, j)
    val listBigramsTrainingENCZ: ListBuffer[(String, String)] = DocumentUtils.documentToBigrams(trainingDataENCZ)

    // Lists of trigrams (i, j, k)
    val listTrigramsTrainingENCZ: ListBuffer[(String, String, String)] = DocumentUtils.documentToTrigrams(trainingDataENCZ)
    val listTrigramsHeldOutENCZ: ListBuffer[(String, String, String)] = DocumentUtils.documentToTrigrams(heldOutDataENCZ)
    val listTrigramsTestENCZ: ListBuffer[(String, String, String)] = DocumentUtils.documentToTrigrams(testDataENCZ)

    // Useful maps
    // Number of occurrences of each word in their respective documents Map(i => Int) = Map((i, _) => Int)
    // Consider the element "", which is at the beginning and the end of the document
    val mapOccurrencesWordsTrainingENCZ: Map[String, Int] = DocumentUtils.getNumberIterationsEachObject(trainingDataENCZ) + ("" -> 2)

    // Number of occurrences of each bigram in their respective documents Map((i, j) => Int)
    // Consider the element ("", ""), which is at the beginning and the end of the document (because we consider trigrams)
    val mapOccurrencesBigramsTrainingENCZ: Map[(String, String), Int] = DocumentUtils.getNumberIterationsEachObject(listBigramsTrainingENCZ) + (("", "") -> 2)

    // Number of occurrences of each trigram in their respective documents Map((i, j) => Int)
    val mapOccurrencesTrigramsTrainingENCZ: Map[(String, String, String), Int] = DocumentUtils.getNumberIterationsEachObject(listTrigramsTrainingENCZ)

    // Useful values
    val vocabularySizeTrainingENCZ = listDistinctWordsTrainingENCZ.length

    // Here comes the coding: extract word counts from the training data so that you are ready to compute unigram-,
    // bigram- and trigram-based probabilities from them; compute also the uniform probability based on the vocabulary
    // size. Remember (T being the text size, and V the vocabulary size, i.e. the number of types - different word
    // forms found in the training text):

    // p0(wi) = 1/V
    def p0(listDistinctWords: ListBuffer[String], vocabularySize: Int): Double =
      1.0 / vocabularySize

    // p1(wi) = c1(wi)/T
    def p1(word: String, mapOccurrencesWords: Map[String, Int], textSize: Int): Double =
      if(mapOccurrencesWords(word) == 0) 0.0
      else mapOccurrencesWords(word).toDouble / textSize

    // p2(wi∣wi−1) = c2(wi−1, wi) / c1(wi−1)
    def p2(previousWord: String, word: String, mapOccurrencesWords: Map[String, Int],
           mapOccurrencesBigrams: Map[(String, String), Int]): Double =
      if(mapOccurrencesBigrams((previousWord, word)) == 0) 0.0
      else mapOccurrencesBigrams((previousWord, word)).toDouble / mapOccurrencesWords(previousWord)

    // p3(wi∣wi−2,wi−1) = c3(wi−2, wi−1, wi) / c2(wi−2, wi−1)​
    def p3(previousPreviousWord: String, previousWord: String, word: String,
           mapOccurrencesBigrams: Map[(String, String), Int],
           mapOccurrencesTrigrams: Map[(String, String, String), Int]): Double =
      if(mapOccurrencesTrigrams((previousPreviousWord, previousWord, word)) == 0) 0.0
      else mapOccurrencesTrigrams((previousPreviousWord, previousWord, word)).toDouble / mapOccurrencesBigrams((previousWord, word))

    // Now compute the four smoothing parameters (i.e. "coefficients", "weights", "lambdas", "interpolation parameters"
    // or whatever, for the trigram, bigram, unigram and uniform distributions) from the held-out data using the EM
    // algorithm. (Then do the same using the training data again: what smoothing coefficients have you got? After
    // answering this question, throw them away!) Remember, the smoothed model has the following form:

    // ps(wi ∣ wi−2,wi−1) = l0p0(wi) + l1p1(wi) + l2p2(wi ∣ wi−1) + l3p3(wi ∣ wi−2, wi−1)
    // where l0 + l1 + l2 + l3​= 1
    def ps(previousPreviousWord: String, previousWord: String, word: String, // ps
           l0: Double, l1: Double, l2: Double, l3: Double, // ps
           listDistinctWords: ListBuffer[String], vocabularySize: Int, // p0
           mapOccurrencesWords: Map[String, Int], textSize: Int, // p1
           mapOccurrencesBigrams: Map[(String, String), Int], // p2
           mapOccurrencesTrigrams: Map[(String, String, String), Int]): Double = // p3

      l0 * p0(listDistinctWords, vocabularySize) +
      l1 * p1(word, mapOccurrencesWords, textSize) +
      l2 * p2(previousWord, word, mapOccurrencesWords, mapOccurrencesBigrams) +
      l3 * p3(previousPreviousWord, previousWord, word, mapOccurrencesBigrams, mapOccurrencesTrigrams)

    // EM algorithm. Returns the values of l0, l1, l2 and l3 in this order.
    def EMAlgorithm(epsilon: Double,
                    listDistinctWords: ListBuffer[String], vocabularySize: Int, // p0
                    mapOccurrencesWords: Map[String, Int], textSize: Int, // p1
                    mapOccurrencesBigrams: Map[(String, String), Int], // p2
                    mapOccurrencesTrigrams: Map[(String, String, String), Int], //p3
                    listTrigrams: ListBuffer[(String, String, String)]): (Double, Double, Double, Double) = {
      // First, initialize all the values at 0.25 (this is arbitrary)
      var l0: Double = 0.25
      var l1: Double = 0.25
      var l2: Double = 0.25
      var l3: Double = 0.25

      // Just an alias to call ps without having to write all the parameters
      def fasterPs(word1: String, word2: String, word3: String): Double = ps(word1, word2, word3, l0, l1, l2, l3, listDistinctWords,
        vocabularySize, mapOccurrencesWords, textSize, mapOccurrencesBigrams, mapOccurrencesTrigrams)

      @scala.annotation.tailrec
      def rec(): Unit = {
        // Expected counts
        val cl0: Double = listTrigrams.map {
          case (word1, word2, word3) => l0 * p0(listDistinctWords, vocabularySize) / fasterPs(word1, word2, word3)
        }.sum

        val cl1: Double = listTrigrams.map {
          case (word1, word2, word3) => l1 * p1(word3, mapOccurrencesWords, textSize) / fasterPs(word1, word2, word3)
        }.sum

        val cl2: Double = listTrigrams.map {
          case (word1, word2, word3) => l2 * p2(word2, word3, mapOccurrencesWords, mapOccurrencesBigrams) / fasterPs(word1, word2, word3)
        }.sum

        val cl3: Double = listTrigrams.map {
          case (word1, word2, word3) => l3 * p3(word1, word2, word3, mapOccurrencesBigrams, mapOccurrencesTrigrams) / fasterPs(word1, word2, word3)
        }.sum


        // Next lambda
        val sumExpectedCounts: Double = cl0 + cl1 + cl2 + cl3

        val nextL0: Double = cl0 / sumExpectedCounts
        val nextL1: Double = cl1 / sumExpectedCounts
        val nextL2: Double = cl2 / sumExpectedCounts
        val nextL3: Double = cl3 / sumExpectedCounts

        // Checking that the sum of the lambdas is indeed 1
        if (l0 + l1 + l2 + l3 < 0.99 || l0 + l1 + l2 + l3 > 1.01) throw new IllegalStateException(s"The sum of the lambdas is not 1! $l0 + $l1 + $l2 + $l3 = ${l0 + l1 + l2 + l3}")

        // Check if we continue
        // The program stops when |nextLi - li| < epsilon for every i
        def compare(a: Double, b: Double): Boolean = Math.abs(a - b) < epsilon

        val continue = if(compare(nextL0, l0) && compare(nextL1, l1) && compare(nextL2, l2) && compare(nextL3, l3)) false else true

        // Affecting to li the values of nextLi
        l0 = nextL0
        l1 = nextL1
        l2 = nextL2
        l3 = nextL3

        // Tail-recursive call
        if(continue) rec()
      }

      // Call the recursion
      rec()

      // Return value
      (l0, l1, l2, l3)
    }

    val liENCZ: (Double, Double, Double, Double) = EMAlgorithm(0.00000001,
      listDistinctWordsTrainingENCZ, vocabularySizeTrainingENCZ, mapOccurrencesWordsTrainingENCZ, heldOutDataLength,
      mapOccurrencesBigramsTrainingENCZ, mapOccurrencesTrigramsTrainingENCZ, 
      listTrigramsHeldOutENCZ)

    println(s"Parameters for the held-out data: $liENCZ")

    val liTrainingENCZ: (Double, Double, Double, Double) = EMAlgorithm(0.00000001,
      listDistinctWordsTrainingENCZ, vocabularySizeTrainingENCZ, mapOccurrencesWordsTrainingENCZ, heldOutDataLength,
      mapOccurrencesBigramsTrainingENCZ, mapOccurrencesTrigramsTrainingENCZ, 
      listTrigramsTrainingENCZ)

    println(s"Parameters for the training data: $liTrainingENCZ")


    // Cross-entropy
    //-(1/|D|)SUMi=1..|D|log2(ps(wi|hi))

    def crossEntropy(testDataLength: Int, listTrigramsTest: ListBuffer[(String, String, String)],
                     li: (Double, Double, Double, Double), listDistinctWordsTraining: ListBuffer[String],
                     vocabularySizeTraining: Int, mapOccurrencesWordsTraining: Map[String, Int], trainingDataLength: Int,
                     mapOccurrencesBigramsTraining: Map[(String, String), Int],
                     mapOccurrencesTrigramsTraining: Map[(String, String, String), Int]): Double =
      (-1.0 / testDataLength) * listTrigramsTest.map {
      case (word1, word2, word3) => NLPUtils.log2(
        ps(word1, word2, word3, li._1, li._2, li._3, li._4, listDistinctWordsTraining, vocabularySizeTraining,
          mapOccurrencesWordsTraining, trainingDataLength, mapOccurrencesBigramsTraining,
          mapOccurrencesTrigramsTraining)
      )
    }.sum

    (-1.0 / testDataLengthENCZ) * listTrigramsTestENCZ.map {
      case (word1, word2, word3) => NLPUtils.log2(
        ps(word1, word2, word3, liENCZ._1, liENCZ._2, liENCZ._3, liENCZ._4, listDistinctWordsTrainingENCZ, vocabularySizeTrainingENCZ,
          mapOccurrencesWordsTrainingENCZ, trainingDataLengthENCZ, mapOccurrencesBigramsTrainingENCZ,
          mapOccurrencesTrigramsTrainingENCZ)
      )
    }.sum



    val crossEntropyENCZ = crossEntropy(testDataLengthENCZ, listTrigramsTestENCZ, liENCZ, listDistinctWordsTrainingENCZ,
      vocabularySizeTrainingENCZ, mapOccurrencesWordsTrainingENCZ, trainingDataLengthENCZ, mapOccurrencesBigramsTrainingENCZ, mapOccurrencesTrigramsTrainingENCZ)

    println(s"Non-tweaked cross-entropy: $crossEntropyENCZ")

    // Now tweak the smoothing parameters in the following way: add 10%, 20%, 30%, ..., 90%, 95% and 99% of the
    // difference between the trigram smoothing parameter and 1.0 to its value, discounting at the same the remaining
    // three parameters proportionally (remember, they have to sum up to 1.0!!).

    val increase = List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
    val difference = 1.0 - liENCZ._4
    
    for(messUp <- increase) {
      val change =  messUp * difference
      val l3 = liENCZ._4 + change

      val total = liENCZ._1 + liENCZ._2 + liENCZ._3

      val l0 = liENCZ._1 - change * liENCZ._1 / total
      val l1 = liENCZ._2 - change * liENCZ._2 / total
      val l2 = liENCZ._3 - change * liENCZ._3 / total

      println(crossEntropy(testDataLengthENCZ, listTrigramsTestENCZ, (l0, l1, l2, l3), listDistinctWordsTrainingENCZ,
        vocabularySizeTrainingENCZ, mapOccurrencesWordsTrainingENCZ, trainingDataLengthENCZ, mapOccurrencesBigramsTrainingENCZ,
        mapOccurrencesTrigramsTrainingENCZ))
    }

    // Then set the trigram smoothing parameter to 90%, 80%, 70%, ... 10%, 0% of its value, boosting proportionally the other three parameters,
    // again to sum up to one.

    val decrease = List(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0)

    for(messUp <- decrease) {
      val change =  1.0 - liENCZ._4 * messUp
      val l3 = liENCZ._4 * messUp

      val total = liENCZ._1 + liENCZ._2 + liENCZ._3

      val l0 = liENCZ._1 + change * liENCZ._1 / total
      val l1 = liENCZ._2 + change * liENCZ._2 / total
      val l2 = liENCZ._3 + change * liENCZ._3 / total

      println(crossEntropy(testDataLengthENCZ, listTrigramsTestENCZ, (l0, l1, l2, l3), listDistinctWordsTrainingENCZ,
        vocabularySizeTrainingENCZ, mapOccurrencesWordsTrainingENCZ, trainingDataLengthENCZ, mapOccurrencesBigramsTrainingENCZ,
        mapOccurrencesTrigramsTrainingENCZ))
    }

    // "Coverage" graph (defined as the percentage of words in the test data which have been seen in the training data).
    var seenWords = 0
    var unseenWords = 0
    testDataENCZ.foreach(word => if(trainingDataENCZ.contains(word)) seenWords = seenWords + 1 else unseenWords = unseenWords + 1)

    println(s"$seenWords / $unseenWords / $testDataLengthENCZ / ${seenWords.toDouble/testDataLengthENCZ}")

    val duration = (System.nanoTime - startTime) / 1e9d
    val seconds = FileUtils.compressedDouble(duration % 60)
    val minutes = FileUtils.compressedDouble(BigDecimal(duration / 60).setScale(0, BigDecimal.RoundingMode.DOWN).toInt)
    println(s"Execution of the program took $minutes minute(s) and $seconds seconds.")
  }
}
