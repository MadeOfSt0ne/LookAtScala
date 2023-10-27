package org.example

object WordScore {

  // sortBy
  def rankedWords(wordScore: String => Int,
                  words: List[String]): List[String] = {
    words.sortBy(wordScore).reverse
  }

  // func passing
  def score(word: String): Int = word.replaceAll("a", "").length

  def scoreWithBonus(word: String): Int = {
    val base = score(word)
    if (word.contains("c")) base + 5 else base
  }

  def bonus(word: String): Int = {
    if (word.contains("c")) 5 else 0
  }

  def penalty(word: String): Int = if (word.contains("s")) 7 else 0

  def len(word: String): Int = word.length

  def numberOfS(s: String): Int = s.length - s.replaceAll("s", "").length

  def negative(i: Int): Int = -i

  def negativeNumberOfS(s: String): Int = -numberOfS(s)

  // map
  def wordScores(wordScore: String => Int,
                 words: List[String]): List[Int] = {
    words.map(wordScore)
  }

  // filter
  def highScoringWords(wordScore: String => Int,
                       words: List[String]): List[String] = {
    words.filter(word => wordScore(word) > 1)
  }
}
