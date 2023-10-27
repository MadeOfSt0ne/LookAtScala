package org.example

object Lists {

  def replan(plan: List[String],
              newCity: String,
              beforeCity: String): List[String] = {
    val beforeCityIndex = plan.indexOf(beforeCity)
    val citiesBefore = plan.slice(0, beforeCityIndex)
    val citiesAfter = plan.slice(beforeCityIndex, plan.size)
    citiesBefore.appended(newCity).appendedAll(citiesAfter)
  }

  def firstTwo(list: List[String]): List[String] = {
    list.slice(0, 2)
  }

  def lastTwo(list: List[String]): List[String] = {
    list.slice(list.size - 2, list.size)
  }

  def insertBeforeLast(list: List[String], ins: String): List[String] = {
    val beforeInsert = list.slice(0, list.size - 1)
    val after = list.slice(list.size - 1, list.size)
    beforeInsert.appended(ins).appendedAll(after)
  }

  def movedFirstTwoToTheEnd(list: List[String]): List[String] = {
    val firstTwo = list.slice(0, 2)
    val withoutTwo = list.slice(2, list.size)
    withoutTwo.appendedAll(firstTwo)
  }

}
