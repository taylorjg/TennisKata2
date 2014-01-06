package jt.katas.tennis

/**
 * Created by Jonathan Taylor on 06/01/2014.
 */
trait Game {
  def winner: Option[Player] = None
  def advantage: Option[Player] = None
  def deuce: Boolean = false
  def pointScoredBy(p: Player): Game
}

case class NewGame(p1: Player, p2: Player) extends Game {
  def pointScoredBy(p: Player) = {
    val s1 = if (p == p1) 1 else 0
    val s2 = if (p == p2) 1 else 0
    (s1, s2) match {
      case (0, 0) => throw new UnsupportedOperationException("Unknown player!")
      case _ => SimpleGame(p1, p2, s1, s2)
    }
  }
}

case class SimpleGame(p1: Player, p2: Player, s1: Int, s2: Int) extends Game {
  def pointScoredBy(p: Player) = {
    val ns1 = s1 + (if (p == p1) 1 else 0)
    val ns2 = s2 + (if (p == p2) 1 else 0)
    (ns1, ns2) match {
      case (3, 3) => DeuceGame(p1, p2)
      case (4, 3) => AdvantageGame(p1, p2, p)
      case (3, 4) => AdvantageGame(p1, p2, p)
      case (4, _) => WonGame(p)
      case (_, 4) => WonGame(p)
      case (x, y) if x == s1 && y == s2 => throw new UnsupportedOperationException("Unknown player!")
      case _ => SimpleGame(p1, p2, ns1, ns2)
    }
  }
}

case class DeuceGame(p1: Player, p2: Player) extends Game {
  override def deuce = true
  def pointScoredBy(p: Player) =
    p match {
      case _ if p == p1 || p == p2 => AdvantageGame(p1, p2, p)
      case _ => throw new UnsupportedOperationException("Unknown player!")
    }
}

case class AdvantageGame(p1: Player, p2: Player, a: Player) extends Game {
  override def advantage = Some(a)
  def pointScoredBy(p: Player) =
    p match {
      case _ if p == p1 || p == p2 => WonGame(p)
      case _ => throw new UnsupportedOperationException("Unknown player!")
    }
}

case class WonGame(w: Player) extends Game {
  override def winner = Some(w)
  def pointScoredBy(p: Player) = {
    throw new UnsupportedOperationException("Game is over!")
  }
}
