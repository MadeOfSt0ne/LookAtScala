package org.example

object Either {

  case class TvShow(title: String, start: Int, end: Int)

  def parseShows(rawShows: List[String]): Either[String, List[TvShow]] = {
    val initialResult: Either[String, List[TvShow]] = Right(List.empty)
    rawShows
      .map(parseShow)
      .foldLeft(initialResult)(addOrResign)
  }

  private def addOrResign(parsedShows: Either[String, List[TvShow]], newParsedShow: Either[String, TvShow]):
    Either[String, List[TvShow]] = {
    for {
      shows      <- parsedShows
      parsedShow <- newParsedShow
    } yield shows.appended(parsedShow)
  }

  private def parseShow(rawShow: String): Either[String, TvShow] = {
    for {
      name      <- extractName(rawShow)
      yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
      yearEnd   <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
    } yield TvShow(name, yearStart, yearEnd)
  }

  private def extractName(rawShow: String): Either[String, String] = {
    val bracketOpen = rawShow.indexOf('(')
    if (bracketOpen > 0)
      Right(rawShow.substring(0, bracketOpen).trim)
    else
      Left(s"Can't extract name from $rawShow")
  }

  private def extractYearStart(rawShow: String): Either[String, Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash        = rawShow.indexOf('-')
    for {
      yearStr <- if (bracketOpen != -1 && dash > bracketOpen + 1)
        Right(rawShow.substring(bracketOpen + 1, dash))
      else Left(s"Can't extract start year from $rawShow")
      year    <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }

  private def extractYearEnd(rawShow: String): Either[String, Int] = {
    val dash = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
      yearStr <- if (dash != -1 && dash + 1 < bracketClose)
        Right(rawShow.substring(dash + 1, bracketClose))
      else Left(s"Can't extract end year from $rawShow")
      year    <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }

  private def extractSingleYear(rawShow: String): Either[String, Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
      yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1) {
        Right(rawShow.substring(bracketOpen + 1, bracketClose))
      } else Left(s"Can't extract single year from $rawShow")
      year    <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }


}
