
class Board {
  private val board = Array.fill[Option[Piece]](6, 9)(None)
  private val padding = new Piece('x', 'x', 'x', 1)
  val placesOnBoard = Array.ofDim[Int](6, 9)

  val padPiece = new Piece('x', 'x', 'x', 1)

  def isFull = this.board.forall( _.forall( _.isDefined))
  def isEmpty = this.board.forall( _.forall( _.getOrElse(this.padding).samePiece(this.padding)))

  def empty() = {
    for {
      i <- this.board.indices
      j <- this.board(i).indices
    } {
      this.board(i)(j) = None
    }
  }
  private def addPlaces() = {
    this.placesOnBoard(0) = Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
    this.placesOnBoard(1) = Array(0, 0, 1, 2, 1, 2, 1, 0, 0)
    this.placesOnBoard(2) = Array(0, 1, 2, 1, 2, 1, 2, 1, 0)
    this.placesOnBoard(3) = Array(0, 2, 1, 2, 1, 2, 1, 2, 0)
    this.placesOnBoard(4) = Array(0, 0, 2, 1, 2, 1, 2, 0, 0)
    this.placesOnBoard(5) = Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

}
