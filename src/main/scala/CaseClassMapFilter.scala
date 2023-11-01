package org.example

object CaseClassMapFilter {

  private case class Book(title: String, authors: List[String])
  case class Movie(title: String)

  {
    val books = List(
      Book("FP in Scala", List("Chiusano", "Bjarnason")),
      Book("The Hobbit", List("Tolkien")),
      Book("Modern Java in Action", List("Urma", "Fusco", "Mycroft"))
    )

    val scalaBooksQty1 = books
      .map(_.title)
      .filter(_.contains("Scala"))
      .size
    assert(scalaBooksQty1 == 1)

    val scalaBooksQty2 = books
      .map(book => book.title)
      .count(title => title.contains("Scala"))
    assert(scalaBooksQty2 == 1)
  }

  private val books = List(
    Book("FP in Scala", List("Chiusano", "Bjarnason")),
    Book("The Hobbit", List("Tolkien"))
  )

  private def bookAdaptations(author: String): List[Movie] = {
    if (author == "Tolkien") List(Movie("An Unexpected Journey"), Movie("The Desolation of Smaug"))
    else List.empty
  }

  val movies = books
    .flatMap(_.authors)
    .flatMap(bookAdaptations)

  // Так сохраняется доступ к книге, автору и фильму
  def recommendationFeed(books: List[Book]) = {
    books.flatMap(book =>
      book.authors.flatMap(author =>
        bookAdaptations(author).map(movie =>
          s"You may like ${movie.title}, " +
          s"because you liked $author's ${book.title}"
        )
      )
    )
  }

  def recommendedBooks(friend: String): List[Book] = {
    val scala = List(
      Book("FP in Scala", List("Chiusano", "Bjarnason")),
      Book("Get Programming with Scala", List("Sfregola"))
    )

    val fiction = List(
      Book("Harry Potter", List("Rowling")),
      Book("The Lord of the Rings", List("Tolkien"))
    )

    if (friend == "Alice") scala
    else if (friend == "Bob") fiction
    else List.empty
  }


}
