package one.xingyi.cddexamples
import one.xingyi.cddcore._
import one.xingyi.cddcore.UntypedScenarioBuilder._
class Tennis {
  val definition = InternetDocument("CodingDojo", "http://codingdojo.org/cgi-bin/wiki.pl?KataTennis")
  val wikipedia = InternetDocument("Wikipedia", "http://en.wikipedia.org/wiki/Tennis_score")
  val changeRequest = InternetDocument("CR24", "http://en.wikipedia.org/wiki/Tennis_score")

  val lookup = Map(0 -> "love", 1 -> "fifteen", 2 -> "thirty", 3 -> "forty")

  val ucLeftWins = UseCase("left winning") { implicit agg: ScenarioAggregator[(Int, Int), String] =>
    scenario(4, 0) produces "left won" when { case (l, r) => (l - r) >= 2 && l >= 4 }
    scenario(4, 1) produces "left won"
    scenario(4, 2) produces "left won"
    scenario(5, 3) produces "left won"
  }
  //  reference("2.1", definition).
  val ucRightWins = UseCase("Receiver winning") { implicit agg: ScenarioAggregator[(Int, Int), String] =>
    scenario(0, 4) produces "right won" when { case (l: Int, r: Int) => (r - l) >= 2 && r >= 4 }
    scenario(1, 4) produces "right won"
    scenario(2, 4) produces "right won"
    scenario(3, 5) produces "right won"
    scenario(40, 42) produces "right won"
  }
  val ucRunningScore = UseCase("Running score") { implicit agg: ScenarioAggregator[(Int, Int), String] => //}, "The running score of each game is described in a manner peculiar to tennis: scores from zero to three points are described as 'love', 'fifteen', 'thirty', and 'forty' respectively.").
    //  reference("2.10", definition)
    scenario(2, 3) produces "thirty, forty" because { case (l: Int, r: Int) if l < 4 && r < 4 => s"${lookup(l)}, ${lookup(r)}" }
    scenario(2, 1) produces "thirty, fifteen"
  }
  val ucXXAll = UseCase("When both have the same running score") { implicit agg: ScenarioAggregator[(Int, Int), String] => //}, "The running score, if both scores are the same, is called xx all").
    //                 reference("2.11", definition).
    scenario(0, 0) produces "love all" because { case (l: Int, r: Int) if l == r && l < 3 => s"${lookup(l)} all" }
    scenario(2, 2) produces "thirty all"

  }
  val ucDeuce = UseCase("Deuce") { implicit agg: ScenarioAggregator[(Int, Int), String] => //.description("If at least three points have been scored by each player, and the scores are equal, the score is 'deuce'.") produces "deuce").priority(1).
    //  reference("5", definition).
    scenario(3, 3) produces "deuce" when { case (l: Int, r: Int) => l >= 3 && r >= 3 && l == r }
    scenario(4, 4) produces "deuce"
    scenario(6, 6) produces "deuce"
  }

  val ucAdvantage = UseCase("Advantage") { implicit agg: ScenarioAggregator[(Int, Int), String] => //.description("If at least three points have been scored by each side and a player has one more point than his opponent, the score of the game is 'advantage' for the player in the lead.").
    //  reference("3", definition).
    scenario(5, 4) produces "advantage left" when { case (l: Int, r: Int) => l >= 3 && r >= 3 && l == r + 1 }
    scenario(6, 5) produces "advantage left" //.reference("2").
    scenario(4, 3) produces "advantage left"

    scenario(4, 5) produces "advantage right" when { case (l: Int, r: Int) => l >= 3 && r >= 3 && r == l + 1 }
    scenario(5, 6) produces "advantage right"
  }

}
