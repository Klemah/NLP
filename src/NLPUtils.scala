import scala.collection.mutable.ListBuffer

object NLPUtils {
  // P(i,j)
  def probability(i: String, j: String, numberOfBigrams: Int, mapBigramToCount: Map[(String, String), Int]): Double = {
    // Divides the number of occurrences of the bigram (i,j) by the total number of bigrams
    mapBigramToCount(i, j).toDouble / numberOfBigrams
  }

  // P(j|i)
  def conditionalProbability(i: String, j: String, mapBigramToCount: Map[(String, String), Int], mapWordToCount: Map[String, Int]): Double = {
    // Divides the number of occurrences of the bigram (i,j) by the number of bigrams starting with the word i
    //println(s"Conditional probability: ${mapBigramToCount(i, j).toDouble} / ${mapWordToCount(i)} = ${mapBigramToCount(i, j).toDouble /  mapWordToCount(i)}")
    mapBigramToCount(i, j).toDouble /  mapWordToCount(i)
  }

  // Conditional entropy
  def conditionalEntropy(listDistinctBigrams: ListBuffer[(String, String)], numberOfBigrams: Int,
                         mapBigramToCount: Map[(String, String), Int], mapWordToCount: Map[String, Int]): Double = {
    var sum: Double = 0.0
    // Computing the sum as in the entropy formula
    var doneI = 0
    for(bigram <- listDistinctBigrams) {
      val i = bigram._1
      val j = bigram._2

      val probIJ: Double = probability(i, j, numberOfBigrams, mapBigramToCount)
      if(probIJ != 0) sum += probIJ * log2(conditionalProbability(i, j, mapBigramToCount, mapWordToCount))
      doneI += 1
      //      println(s"${Utils.displayablePercentage(doneI, listBigrams.length)}% - " +
      //        s"i: $i, j: $j - " +
      //        s"sum: $sum -" +
      //        s"probIJ: $probIJ - " +
      //        s"CP: ${conditionalProbability(i, j, mapBigramToCount, mapWordToCount)} - " +
      //        s"log(CP): ${log2(conditionalProbability(i, j, mapBigramToCount, mapWordToCount))} - " +
      //        s"CE: ${probIJ * log2(conditionalProbability(i, j, mapBigramToCount, mapWordToCount))}")

      if(sum == Double.PositiveInfinity || sum == Double.NegativeInfinity) throw new IllegalStateException("Sum reached Infinity")
    }
    // Applying the minus sign of the conditional entropy formula at the end
    -sum
  }

  def perplexity(conditionalEntropyEN: Double): Double = Math.pow(2, conditionalEntropyEN)

  def log2(x: Double): Double = Math.log10(x)/Math.log10(2.0)
}
