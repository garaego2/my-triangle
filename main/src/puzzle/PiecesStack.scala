package puzzle

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

  def addPiece(p: Piece) = {
    if (!this.stack.exists( _.samePiece(p) )) {
      this.stack += p
      this.current = Some(p)
    }
  }

  def takePiece(p: Piece) = {
    if (this.stack.exists( _.samePiece(p) )) {
      if (p.samePiece(this.current.getOrElse(new Piece('x', 'x', 'x', 1)))) {
        if (this.size == 1) {
          this.current = None
        } else {
          this.flipRight()
        }
      }
      this.stack -= p
    }
  }


  def flipLeft() = {
    if (this.size > 1) {
      val i = this.stack.indexWhere( _.samePiece(this.current.get) )
      if (i == 0) {
        this.current = Some(this.stack(this.size - 1))
      } else this.current = Some(this.stack(i - 1))
    }
  }


  def flipRight() = {
    if (this.size > 1) {
      val i = this.stack.indexWhere( _.samePiece(this.current.get))
      if (i == this.size - 1) {
        this.current = Some(this.stack.head)
      } else this.current = Some(this.stack(i + 1))
    }
  }

  def empty() = {
    if (!this.isEmpty) {
      for (i <- 0 until this.size) {
        this.takePiece(stack.head)
      }
    }
  }
}
