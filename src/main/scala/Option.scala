package org.example

object Option {
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
      validName <- validateName(name)
      validEnd <- validateEnd(end)
      validStart <- validateStart(start, end)
    } yield Event(validName, validStart, validEnd)
  }

  case class TvShow(title: String, start: Int, end: Int)

  def sortShows(shows: List[TvShow]): List[TvShow] = {
    shows
      .sortBy(tvShow => tvShow.end - tvShow.start)
      .reverse
  }

  def parseShows(rawShows: List[String]): List[TvShow] = {
    rawShows.map(parseShow)
  }

  private def parseShow(rawShow: String): TvShow = {
    val bracketOpen  = rawShow.indexOf('(')
    val bracketClose = rawShow.indexOf(')')
    val dash         = rawShow.indexOf('-')

    val name      = rawShow.substring(0, bracketOpen).trim
    val yearStart = Integer.parseInt(rawShow.substring(bracketOpen + 1, dash))
    val yearEnd   = Integer.parseInt(rawShow.substring(dash + 1, bracketClose))

    TvShow(name, yearStart, yearEnd)
  }

  def parseShow2(rawShow: String): Option[TvShow] = {
    for {
      name      <- extractName(rawShow)
      yearStart <- extractYearStart(rawShow)
      yearEnd   <- extractYearEnd(rawShow)
    } yield TvShow(name, yearStart, yearEnd)
  }

  private def extractName(rawShow: String): Option[String] ={
    val bracketOpen = rawShow.indexOf('(')
    if (bracketOpen != -1) Some(rawShow.substring(0, bracketOpen)) else None
  }

  private def extractYearStart(rawShow: String): Option[Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash        = rawShow.indexOf('-')
    for {
      yearStr <- if (bracketOpen != -1 && dash > bracketOpen + 1)
        Some(rawShow.substring(bracketOpen + 1, dash))
      else None
      year <- yearStr.toIntOption
    } yield year
  }

  private def extractYearEnd(rawShow: String): Option[Int] = {
    val dash         = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
      yearStr <- if (dash != -1 && bracketClose + 1 > dash)
        Some(rawShow.substring(dash + 1, bracketClose))
      else None
      year <- yearStr.toIntOption
    } yield year
  }

}
