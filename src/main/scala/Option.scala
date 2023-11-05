package org.example

object Option {

  case class TvShow(title: String, start: Int, end: Int)

  def sortShows(shows: List[TvShow]): List[TvShow] = {
    shows
      .sortBy(tvShow => tvShow.end - tvShow.start)
      .reverse
  }

  def parseShows(rawShows: List[String]): List[TvShow] = {
    rawShows
      .map(parseShow2)
      .flatMap(_.toList)
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
      yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
      yearEnd   <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
    } yield TvShow(name, yearStart, yearEnd)
  }

  private def extractName(rawShow: String): Option[String] = {
    val bracketOpen = rawShow.indexOf('(')
    if (bracketOpen != -1) Some(rawShow.substring(0, bracketOpen).trim) else None
  }

  private def extractYearStart(rawShow: String): Option[Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash        = rawShow.indexOf('-')
    for {
      yearStr <- if (bracketOpen != -1 && dash > bracketOpen + 1)
        Some(rawShow.substring(bracketOpen + 1, dash))
      else None
      year    <- yearStr.toIntOption
    } yield year
  }

  private def extractYearEnd(rawShow: String): Option[Int] = {
    val dash         = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
      yearStr <- if (dash != -1 && bracketClose + 1 > dash)
        Some(rawShow.substring(dash + 1, bracketClose))
      else None
      year    <- yearStr.toIntOption
    } yield year
  }

  private def extractSingleYear(rawShow: String): Option[Int] = {
    val bracketOpen  = rawShow.indexOf('(')
    val dash         = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
      yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
        Some(rawShow.substring(bracketOpen + 1, bracketClose))
      else None
      year    <- yearStr.toIntOption
    } yield year
  }

  // All or nothing
  def addOrResign(parsedShows: Option[List[TvShow]], newParsedShow: Option[TvShow]): Option[List[TvShow]] = {
    for {
      shows      <- parsedShows
      parsedShow <- newParsedShow
    } yield shows.appended(parsedShow)
  }

  // Свёртка списка значений Option в значение Option со списком
  def parseShows3(rawShows: List[String]): Option[List[TvShow]] = {
    val initialResult: Option[List[TvShow]] = Some(List.empty)
    rawShows
      .map(parseShow2)
      .foldLeft(initialResult)(addOrResign)
  }
}
