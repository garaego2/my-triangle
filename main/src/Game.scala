class Game {
  val board = new Board
  val stack = new PiecesStack

  val piecePlaces = board.placesOnBoard

  private var solution = Array.ofDim[Option[Piece]](6, 9)

  private var gameStarted = false

  def sol = this.solution


}
