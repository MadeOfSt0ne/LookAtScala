package org.example

object TipCalculator {
  def main(args: Array[String]): Unit = {
    println("Empty list = " + TipCalculator.getTipPercentage(List.empty))
    println("List of 3 = " + TipCalculator.getTipPercentage(List("Alice", "Bob", "Charlie")))
    println("List of 6 = " + TipCalculator.getTipPercentage(List("Alice", "Bob", "Charlie", "Alice", "Bob", "Charlie")))

    assert(getTipPercentage(List("Alice", "Bob")) == 10)
    assert(getTipPercentage(List("Alice", "Bob", "Charlie", "Danny", "Emily", "Wojtek")) == 20)
    assert(getTipPercentage(List.empty) == 0)
  }

  private def getTipPercentage(names: List[String]): Int = {
    if (names.size > 5) 20
    else if (names.size > 0) 10
    else 0
  }
}


