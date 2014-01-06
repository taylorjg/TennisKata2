package jt.katas.tennis

/**
 * Created by Jonathan Taylor on 06/01/2014.
 */
trait Game {
  def player1Points = 0
  def player2Points = 0
  def winner: Option[Player] = None
  def advantage: Option[Player] = None
  def deuce: Boolean = false
  def pointScoredBy(p: Player): Game
}

case class WonGame(winningPlayer: Player) extends Game {
  override def winner = Some(winningPlayer)
  def pointScoredBy(p: Player) = {
    throw new UnsupportedOperationException("Game is over!")
  }
}

case class AdvantageGame(p1: Player, p2: Player, advantagePlayer: Player) extends Game {
  override def player1Points = if (advantagePlayer == p1) 4 else 3
  override def player2Points = if (advantagePlayer == p2) 4 else 3
  override def advantage = Some(advantagePlayer)
  def pointScoredBy(p: Player) =
    p match {
      case _ if p == p1 || p == p2 => WonGame(p)
      case _ => throw new UnsupportedOperationException("Unknown player!")
    }
}

case class DeuceGame(p1: Player, p2: Player) extends Game {
  override def player1Points = 3
  override def player2Points = 3
  override def deuce = true
  def pointScoredBy(p: Player) =
    p match {
      case _ if p == p1 || p == p2 => AdvantageGame(p1, p2, p)
      case _ => throw new UnsupportedOperationException("Unknown player!")
    }
}

case class SimpleGame(p1: Player, p2: Player, override val player1Points: Int, override val player2Points: Int) extends Game {
  def pointScoredBy(p: Player) = {
    val s1 = player1Points + (if (p == p1) 1 else 0)
    val s2 = player2Points + (if (p == p2) 1 else 0)
    (s1, s2) match {
      case (3, 3) => DeuceGame(p1, p2)
      case (4, 3) => AdvantageGame(p1, p2, p)
      case (3, 4) => AdvantageGame(p1, p2, p)
      case (x, y) if x == player1Points && y == player2Points => throw new UnsupportedOperationException("Unknown player!")
      case _ => SimpleGame(p1, p2, s1, s2)
    }
  }
}
