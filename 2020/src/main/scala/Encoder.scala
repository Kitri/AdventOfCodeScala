object Encoder {

  def findSumInList(input: List[BigInt], sumTotal: BigInt, index: Int, sumList: List[BigInt]): List[BigInt] = {
    if(sumList.sum == sumTotal) sumList
    else if(sumList.sum > sumTotal) findSumInList(input.drop(1), sumTotal, 0, List.empty)
    else findSumInList(input, sumTotal, index + 1, sumList :+ input(index))
  }

  def findXMASMistake(input: List[BigInt], preamble: Int): BigInt = {
    if(input.size <= preamble) 0
    else if(checkSum(input.take(preamble), input(preamble))) findXMASMistake(input.drop(1), preamble)
    else input(preamble)
  }

  def checkSum(input: List[BigInt], value: BigInt): Boolean = {
    input.combinations(2).filter(x => x.head != x(1)).map(_.sum).contains(value)
  }
}
