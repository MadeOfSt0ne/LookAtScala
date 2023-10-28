package org.example

object ReturningFunc {

  def highScoringWords(wordScore: String => Int,
                       words: List[String]): Int => List[String] = {
    higherThan => words.filter(word => wordScore(word) > higherThan)
  }

  def highScoringWords2(wordScore: String => Int): Int => List[String] => List[String] = {
    higherThan =>
      words =>
        words.filter(word => wordScore(word) > higherThan)
  }

  // Currying
  // Каррирование - преобразование функций с несколькими параметрами в серию функций с одним параметром, возвращаемых
  // друг из друга.
  def highScoringWords3(wordScore: String => Int)(higherThan: Int)(words: List[String]): List[String] = {
    words.filter(word => wordScore(word) > higherThan)
  }

  def largerThan(n: Int)(i: Int): Boolean = i > n

  def divisibleBy(n: Int)(i: Int): Boolean = i % n == 0

  def shorterThan(n: Int)(s: String): Boolean = s.length < n

  def numberOfS(s: String): Int = s.length - s.replaceAll("s", "").length

  def containsS(moreThan: Int)(s: String): Boolean = numberOfS(s) > moreThan

  // FoldLeft
  def cumulativeScore(wordScore: String => Int,
                      words: List[String]): Int = {
    words.foldLeft(0)((total, word) => total + wordScore(word))
  }
}
