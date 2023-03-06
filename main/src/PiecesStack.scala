import scala.collection.mutable.Buffer

class PiecesStack {
  private val stack = Buffer[Piece]()
  private var current: Option[Piece] = None
  def pieceStack = this.stack.toVector

}
