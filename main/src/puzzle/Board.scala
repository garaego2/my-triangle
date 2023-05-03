package puzzle

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
  /* Adds a piece on the board. The method returns a Boolean value that tells whether the piece has
   * successfully been added or not. A piece can only be added on the board meaning that the y
   * value can vary between 1 and 4, and the x value between 1 and 5 if y=1 or y=4, or between 1 and 7
   * if y=2 or y=3. The method also returns false if a piece is added to a position that already contains a piece.
   * In each position the triangles can be either tip upwards or tip downwards depending on the position.
   * The method rotates the pieces to the closest suitable position if they are already not correctly positioned. */
    def addPiece(p: Piece, x: Int, y: Int) = {
    if (y >= 1 && y <= 4) {
      if ((y == 1 || y == 4) && (x >= 1 && x <= 5) && this.board(y)(x + 1).isEmpty) {
        if (y == 1 && (x == 1 || x == 3 || x == 5) && p.position%2 == 0) p.rotate()
        if (y == 1 && (x == 2 || x == 4) && p.position%2 == 1) p.rotate()
        if (y == 4 && (x == 1 || x == 3 || x == 5) && p.position%2 == 1) p.rotate()
        if (y == 4 && (x == 2 || x == 4) && p.position%2 == 0) p.rotate()
        this.board(y)(x + 1) = Some(p)
        p.addCoords(y, x)
        true
      } else {
        if ((y == 2 || y == 3 ) && (x >= 1 && x <= 7) && this.board(y)(x).isEmpty) {
          if (y == 2 && (x == 1 || x == 3 || x == 5 || x == 7) && p.position%2 == 0) p.rotate()
          if (y == 2 && (x == 2 || x == 4 || x == 6) && p.position%2 == 1) p.rotate()
          if (y == 3 && (x == 1 || x == 3 || x == 5 || x == 7) && p.position%2 == 1) p.rotate()
          if (y == 3 && (x == 2 || x == 4 || x == 6) && p.position%2 == 0) p.rotate()
          this.board(y)(x) = Some(p)
          p.addCoords(y, x)
          true
        } else false
      }
    } else false
  }
  // Removes piece
  def removePiece(piece: Piece) = {
    val x =
      for {i <- this.board.indices
           j <- this.board(i).indices
        if (this.board(i)(j) == Some(piece))
      } yield (i, j)
    val coords = x.headOption.getOrElse((-1, -1))

    if (coords != (-1, -1) && !piece.samePiece(this.padPiece)) {
      this.board(coords._1)(coords._2) = None
      piece.removeCoords()
      true
    } else false
  }

/* Creates padding around the board since the board is not square. Padding makes it easier to handle
   * the separate positions on the board without having to separately handle the positions on the edges.
   * The padding consists of pieces Piece('x', 'x', 'x', 1), where all of the sides have the char 'x'. */
  private def init() = {
    for {
      i <- this.board.indices
      j <- this.board(i).indices
    } {
      if (i == 0 || i == 5) { this.board(i)(j) = Some(this.padPiece) }
      if ((i == 1 || i == 4) && (j <= 1 || j >= 7)) { this.board(i)(j) = Some(this.padPiece) }
      if ((i == 2 || i == 3) && (j == 0 || j == 8)) { this.board(i)(j) = Some(this.padPiece) }
    }
    this.addPlaces()
  }
  this.init()

  def getBoard = this.board.clone

  def rows = this.board.length
  def columns = this.board(0).length

  def pieceOnCoords(x: Int, y: Int): Option[Piece] = {
    if (y >= 1 && y <= 4) {
      if ((y == 1 || y == 4) && (x >= 1 && x <= 5)) this.board(y)(x + 1)
      else if ((y == 2 || y == 3) && (x >= 1 && x <= 7)) this.board(y)(x)
      else None
    } else None

}
}
