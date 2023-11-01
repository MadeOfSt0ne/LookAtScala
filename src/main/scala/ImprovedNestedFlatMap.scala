package org.example

object ImprovedNestedFlatMap {

  private case class Book(title: String, authors: List[String])
  case class Movie(title: String)

  private def bookAdaptations(author: String): List[Movie] = {
    if (author == "Tolkien") List(Movie("An Unexpected Journey"), Movie("The Desolation of Smaug"))
    else List.empty
  }

  // для каждого объекта из списка выполнить действие
  def recommendationFeed(books: List[Book]) = {
    for {
      book <- books
      author <- book.authors
      movie <- bookAdaptations(author)
    } yield s"You may like ${movie.title}, " +
            s"because you liked $author's ${book.title}"
  }

  case class Point(x: Int, y: Int)
  val points = List(Point(5, 2), Point(1, 1))
  val radiuses = List(2, 1)

  def isInside(point: Point, radius: Int): Boolean = {
    radius * radius >= point.x * point.x + point.y * point.y
  }

  // FILTERING TECHNIQUES

  // using filter
  assert((for {
    r <- radiuses
    point <- points.filter(p => isInside(p, r))
  } yield s"$point is within a radius of $r") == List("Point(1,1) is within a radius of 2"))

  // using a guard expression
  assert((for {
    r <- radiuses
    point <- points
    if isInside(point, r)
  } yield s"$point is within a radius of $r") == List("Point(1,1) is within a radius of 2"))

  // using flatMap
  def insideFilter(point: Point, radius: Int): List[Point] = if (isInside(point, radius)) List(point) else List.empty

  assert((for {
    r <- radiuses
    point <- points
    inPoint <- insideFilter(point, r)
  } yield s"$inPoint is within a radius of $r") == List("Point(1,1) is within a radius of 2"))
}
