import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}

object FileUtils {
  // Safe opening of files
  def safeOpenFile(path: String): BufferedSource = {
    try {
      Source.fromFile(path)
    }

    catch {
      case _: FileNotFoundException =>
        throw new FileNotFoundException(s"Error: the file '$path' could not be opened!\n Current path: ${System.getProperty("user.dir")}")
    }
  }

  // Safe closing of files
  def safeCloseFile(file: BufferedSource): Unit = {
    if(file != null) file.close
  }

  // Get the list of the lines from a file
  def getListLines(path: String): ListBuffer[String] = {
    val file: BufferedSource = FileUtils.safeOpenFile(path)
    val listLines: ListBuffer[String] = file.getLines.to(collection.mutable.ListBuffer)
    FileUtils.safeCloseFile(file)

    listLines
  }

  // Used for printing
  def compressedDouble(double: Double): Float = {
    BigDecimal(double).setScale(1, BigDecimal.RoundingMode.HALF_DOWN).toFloat
  }

  // Used for printing
  def displayablePercentage(done: Int, toDo: Int): Float = {
    compressedDouble(100.0*done/toDo)
  }

  // Writes the lines "lines" to the file "fileName"
  def writeFile(fileName: String, lines: Seq[String]): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    for(line <- lines) {
      bw.write(line)
      bw.write("\n")
    }
    bw.close()
  }
}
