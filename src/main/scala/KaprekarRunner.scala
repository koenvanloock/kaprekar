object KaprekarRunner {


  def main(args: Array[String]): Unit = {
    print(calculateStep(1235).reverse.mkString("\n"))
  }

  def calculateStep(number: Int, previousSteps: List[ResultLine] = Nil): List[ResultLine] = {
    val digits = number.toString.split("").map(_.toInt)
    val ascNumber =  digitArrayToNumber(digits.sorted)
    val descNumber =  digitArrayToNumber(digits.sorted.reverse)
    val result = descNumber - ascNumber;
    if(previousSteps.nonEmpty && previousSteps.head.result == result) previousSteps
    else calculateStep(result, ResultLine(descNumber, ascNumber, result) :: previousSteps)
  }

  private def digitArrayToNumber(descDigits: Array[Int]) = {
    descDigits.zipWithIndex.map { case (digit: Int, index: Int) => (digit * Math.pow(10, 3 - index)).toInt }.sum
  }
}

case class ResultLine(descNumber: Int, ascNumber: Int, result: Int) {

  override
  def toString = f"$descNumber%04d - $ascNumber%04d = $result%04d"
}
