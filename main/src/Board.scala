
class Board {
  private val board = Array.fill[Option[Piece]](6, 9)(None)
  private val padding = new Piece('x', 'x', 'x', 1)

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

}
