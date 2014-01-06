package jt.katas.tennis

import org.scalatest.{BeforeAndAfter, FunSpec}

/**
 * Created by Jonathan Taylor on 06/01/2014.
 */
class ScoreFormatterSpec extends FunSpec with BeforeAndAfter {

  val p1 = new Player("Becker")
  val p2 = new Player("McEnroe")
  val bogusPlayer = new Player("Azarenka")
  val newGame = NewGame(p1, p2)
  val deuceGame = newGame
    .pointScoredBy(p1)
    .pointScoredBy(p1)
    .pointScoredBy(p1)
    .pointScoredBy(p2)
    .pointScoredBy(p2)
    .pointScoredBy(p2)

  describe("Tennis score") {

    it("should initially return '0/0'") {
      val actual = ScoreFormatter.score(newGame)
      assertResult("0/0")(actual)
    }

    it("should throw UnsupportedOperationException if a bogus player scores a point in a new game") {
      intercept[UnsupportedOperationException] {
        newGame.pointScoredBy(bogusPlayer)
      }
    }

    it("should return '15/0' after player1 has score a point") {
      val actual = ScoreFormatter.score(newGame.pointScoredBy(p1))
      assertResult("15/0")(actual)
    }

    it("should return '30/40' after player1 has scored 2 points and player2 has scored 3 points") {
      val actual = ScoreFormatter.score(
        newGame
          .pointScoredBy(p1)
          .pointScoredBy(p1)
          .pointScoredBy(p2)
          .pointScoredBy(p2)
          .pointScoredBy(p2))
      assertResult("30/40")(actual)
    }

    it("should throw UnsupportedOperationException if a bogus player scores a point in a simple game") {
      intercept[UnsupportedOperationException] {
        newGame
          .pointScoredBy(p1)
          .pointScoredBy(bogusPlayer)
      }
    }

    it("should return 'deuce' at deuce") {
      val actual = ScoreFormatter.score(deuceGame)
      assertResult("deuce")(actual)
    }

    it("should throw UnsupportedOperationException if a bogus player scores a point at deuce") {
      intercept[UnsupportedOperationException] {
        deuceGame.pointScoredBy(bogusPlayer)
      }
    }

    it("should return 'advantage: McEnroe' after player2 scores a point after deuce") {
      val actual = ScoreFormatter.score(deuceGame.pointScoredBy(p2))
      assertResult("advantage: McEnroe")(actual)
    }

    it("should throw UnsupportedOperationException if a bogus player scores a point at advantage") {
      intercept[UnsupportedOperationException] {
        deuceGame
          .pointScoredBy(p1)
          .pointScoredBy(bogusPlayer)
      }
    }

    it("should return 'winner: Becker' after a love game to player1") {
      val actual = ScoreFormatter.score(
        newGame
          .pointScoredBy(p1)
          .pointScoredBy(p1)
          .pointScoredBy(p1)
          .pointScoredBy(p1))
      assertResult("winner: Becker")(actual)
    }

    it("should return 'winner: McEnroe' after a love game to player2") {
      val actual = ScoreFormatter.score(
        newGame
          .pointScoredBy(p2)
          .pointScoredBy(p2)
          .pointScoredBy(p2)
          .pointScoredBy(p2))
      assertResult("winner: McEnroe")(actual)
    }

    it("should throw UnsupportedOperationException if a player scores a point when the game is already won") {
      intercept[UnsupportedOperationException] {
        deuceGame
          .pointScoredBy(p1)
          .pointScoredBy(p1)
          .pointScoredBy(p1)
      }
    }
  }
}
