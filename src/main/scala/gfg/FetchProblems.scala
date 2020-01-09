package gfg

object FetchProblems {

  def main(args: Array[String]): Unit = {
    val category = "recursion"
    val count = 3
    getQuestions(category, count)
    //    printComments(getQuestions(category, 3).reverse)
  }

  def getQuestions(category: String, count: Int): List[String] = {
    val urls = (2 to count).toList.map("http://www.geeksforgeeks.org/category/" + category + "/page/" + _ + "/")
    val allUrls = "http://www.geeksforgeeks.org/category/" + category + "/" :: urls
    val pages = allUrls.map(scala.io.Source.fromURL(_).mkString)
    print(pages)
    pages.flatMap(_.split("\r\n").filter(_.contains("bookmark"))
      .map(_.split("bookmark\">").last).map(_.split("<\\/").head))
  }

  def printComments(questions: List[String]): Unit = {
    for (i <- 1 to questions.length) {
      println("  /**")
      println("   * " + i + ".\n   * Problem: " + questions(i - 1) + ".")
      println("   * Solution: ")
      println("   */")
      println("")
    }
  }

}