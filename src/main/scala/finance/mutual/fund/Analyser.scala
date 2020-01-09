package finance.mutual.fund

object Analyser {

  case class NAV(curr: Double, max: Double, maxDate: String, min: Double, minDate: String)

  def main(args: Array[String]): Unit = {
//    val fundCategories = List("elss", "large-cap", "small-and-mid-cap", "diversified-equity", "thematic-infrastructure",
//      "sector-pharma-and-healthcare", "sector-banking-and-finance", "sector-fmcg", "sector-technology", "sector-others"
//    )
    val fundCategories = List("elss")
    val result = fundCategories.foldLeft(Map[String, (Double, Double)]())(_ ++ getFundScores(_))
    val sortedMapResults = result.toSeq.sortBy { case (_, (_, b)) => b }
    val returnResults = sortedMapResults.toList.reverse.take(10)
    val navResults = fundCategories.foldLeft(Map[String, NAV]())(_ ++ getNavDetails(_))
    val fundToInvest =
      returnResults.map{case(k,v) => (k,v, normalizeNav(navResults(k).curr, navResults(k).max), navResults(k))}
    fundToInvest.foreach(println)
  }

  def normalizeNav(curr: Double, max: Double): Double = 100 - ((curr / max) * 100)

  def getNavDetails(fundCategory: String) : Map[String, NAV]  = {
    val navUrl = "http://www.moneycontrol.com/mutual-funds/performance-tracker/navs/" + fundCategory + ".html"
    val lines = scala.io.Source.fromURL(navUrl).mkString.split("\n").filter(isNavLine).toList
    getNavMap(lines)
  }

  def getNavMap(lines: List[String]): Map[String, NAV] = {
    lines.grouped(6).filter(_.length == 6).map(returnLines => {
      getFundName(returnLines.head) -> getNav(returnLines.tail)
    }).toMap
  }

  def getNav(strings: List[String]): NAV = {
    NAV(
      try { strings.head.split(">")(1).split("<").head.toDouble } catch { case _ => 0 },
      try { strings(1).split(">")(1).split("<").head.toDouble } catch { case _ => 0 },
      strings(2).split(">")(1).split("<").head,
      try { strings(3).split(">")(1).split("<").head.toDouble } catch { case _ => 0 },
      strings(4).split(">")(1).split("<").head
    )
  }

  def getFundScores(fundCategory: String): Map[String, (Double, Double)] = {
    val url = "http://www.moneycontrol.com/mutual-funds/performance-tracker/returns/" + fundCategory + ".html"
    val lines = scala.io.Source.fromURL(url).mkString.split("\n").filter(isFundLine).toList
    getReturnsMap(lines)
  }

  def isFundLine(line: String): Boolean =
    line.contains("class='bl_12") ||
      line.contains("class='grey12") ||
      line.contains("<td width='60' align='right'>") ||
      line.contains("class='red12") ||
      line.contains("class='grn12")

  def isNavLine(line: String): Boolean =
    line.contains("class='bl_12") ||
      line.contains("<td width='50' align='right'>") ||
      line.contains("<td width='60' align='right'>") ||
      line.contains("<td width='95' align='center'>")

  def getReturnsMap(lines: List[String]): Map[String, (Double, Double)] = {
    lines.grouped(9).filter(_.length == 9).map(returnLines => {
      getFundName(returnLines.head) -> getScore(returnLines(1) :: returnLines.takeRight(4))
    }).toMap
  }

  def getFundName(line: String): String = {
    line.split("class='bl_12'>").last.split("<").head
  }

  def getScore(lines: List[String]): (Double, Double) = {
    val returns = getYearlyReturnsList(lines)
    val piece = 100 / 8
    val score = (returns(1) * piece + returns(2) * 2 * piece + returns(3) * 3 * piece + returns(4) * 4 * piece) / 100
    (returns.head, score)
  }

  def getYearlyReturnsList(lines: List[String]): List[Double] = {
    lines.map(line => {
      val value = line.split(">").last.split("<").head.replaceAll("\"", "").filterNot(_ == ',')
      try {
        value.toDouble
      } catch {
        case _ => 0
      }
    })
  }

}
