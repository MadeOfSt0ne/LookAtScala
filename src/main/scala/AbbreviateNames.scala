package org.example

object AbbreviateNames {

  private def abbreviate(name: String): String = {
    val initial = name.substring(0, 1)
    val separator = name.indexOf(' ')
    val lastName = name.substring(separator + 1)
    initial + ". " + lastName
  }

  def main(args: Array[String]): Unit = {
    assert(abbreviate("Alonzo Church") == "A. Church")
    assert(abbreviate("A. Church") == "A. Church")
    assert(abbreviate("A Church") == "A. Church")
  }
}
