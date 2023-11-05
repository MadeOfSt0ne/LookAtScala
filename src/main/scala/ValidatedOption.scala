package org.example

object ValidatedOption {

  case class Event(name: String, start: Int, end: Int)

  def parse(name: String, start: Int, end: Int): Option[Event] = {
    if (name.size > 0 && end < 3000 && start <= end)
      Some(Event(name, start, end))
    else
      None
  }

  private def validateName(name: String): Option[String] =
    if (name.size > 0) Some(name) else None

  private def validateEnd(end: Int): Option[Int] =
    if (end < 3000) Some(end) else None

  private def validateStart(start: Int, end: Int): Option[Int] =
    if (start <= end) Some(start) else None

  def parse2(name: String, start: Int, end: Int): Option[Event] = {
    for {
      validName  <- validateName(name)
      validEnd   <- validateEnd(end)
      validStart <- validateStart(start, end)
    } yield Event(validName, validStart, validEnd)
  }

}
