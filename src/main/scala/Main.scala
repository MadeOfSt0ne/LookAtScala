package org.example

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(increment(5))
    println(getFirstCharacter("scala"))
    println(wordScore("word"))
  }

  private def increment(x: Int): Int = {
    x + 1
  }

  private def getFirstCharacter(s: String): Char = {
    s.charAt(0)
  }

  private def wordScore(word: String): Int = {
    word.length
  }
}