import scala.collection.mutable.Buffer

class PiecesStack {
  private val stack = Buffer[Piece]()
  private var current: Option[Piece] = None
  def pieceStack = this.stack.toVector
  def currentPiece = this.current

  def piecePile = this.stack.toVector

  def contains(p: Piece): Boolean = this.stack.exists( _.samePiece(p) )

  def isEmpty: Boolean = this.stack.isEmpty

  def size = this.stack.size

  def pieceOnIndex(i: Int): Option[Piece] = if (this.size - 1 >= i) Some(this.stack(i)) else None

}
