package jt.katas.tennis

/**
 * Created by Jonathan Taylor on 06/01/2014.
 */
object ScoreFormatter {

  def score(game: Game) =
    game match {
      case NewGame(_, _) => formatSimpleScores(0, 0)
      case SimpleGame(_, _, s1, s2) => formatSimpleScores(s1, s2)
      case DeuceGame(_, _) => "deuce"
      case AdvantageGame(_, _, a) => "advantage: %s".format(a.name)
      case WonGame(w) => "winner: %s".format(w.name)
    }

  private def formatSimpleScores(s1: Int, s2: Int) = {
    "%s/%s".format(
      formatSimpleScore(s1),
      formatSimpleScore(s2))
  }

  private def formatSimpleScore(score: Int) =
    score match {
      case 0 => "0"
      case 1 => "15"
      case 2 => "30"
      case 3 => "40"
      case _ => "???"
    }
}
