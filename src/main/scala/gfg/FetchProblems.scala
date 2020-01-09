package gfg

object FetchProblems {

  def main(args: Array[String]): Unit = {
    val category = "data-structures/binary-search-tree"
        printComments(getQuestions(category, 1).reverse)
//    getQuestionsOrderedByVotes(category, 11, 50).foreach(println)
  }

  def getQuestions(category: String, count: Int): List[String] = {
    val urls = (2 to count).toList.map("https://www.geeksforgeeks.org/category/" + category + "/page/" + _ + "/")
    val allUrls = "https://www.geeksforgeeks.org/category/" + category + "/" :: urls
    val pages = allUrls.map(scala.io.Source.fromURL(_).mkString)
    pages.flatMap(_.split("\r\n").filter(_.contains("bookmark"))
      .map(_.split("bookmark\">").last).map(_.split("<\\/").head))
  }

  def printComments(questions: List[String]): Unit = {
    for (i <- 1 to questions.length) {
      print("  /**")
      print(" " + i + ". Problem: " + questions(i - 1) + ". */ \n")
      println("")
    }
  }

  def getQuestionsOrderedByVotes(category: String, count: Int, topCount: Int): List[(String, Int)] = {
    val urls = (2 to count).toList.map("https://www.geeksforgeeks.org/category/" + category + "/page/" + _ + "/")
    val allUrls = "https://www.geeksforgeeks.org/category/" + category + "/" :: urls
    val pages = allUrls.map(scala.io.Source.fromURL(_).mkString)
    val res = pages.flatMap{ e =>
      getVoteCount(e)
    }
    res.sortWith((e1, e2) => e1._2 > e2._2).take(topCount)
  }

  def getVoteCount(str: String): Array[(String, Int)] = {
    val res = str.split("\r\n").filter(_.contains("bookmark")).map{ e =>
      val title = e.split("bookmark\">").last.split("<\\/").head
      val link = e.split("href=\"").last.split("\\/\"").head
      (title, getCount(link))
    }
    res
  }

  def getCount(str: String): Int = {
    val res = scala.io.Source.fromURL(str).mkString
    val upvote = res.split("\r\n").filter(_.contains("upvoteText")).head.split("upvoteText\">").last.split("<\\/span").head
    if (upvote.contains("upvote")) 0 else upvote.toInt
  }

}