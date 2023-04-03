class Piece(val left: Char, val bottom: Char, val right: Char, private var pos: Int) {
  private var coords: Option[(Int, Int)] = None
  def position = this.pos

  def getCoords = this.coords.getOrElse(-1, -1)

  def addCoords(y: Int, x: Int) = this.coords = Some(y, x)

  def removeCoords() = this.coords = None

  def rotate() = if (this.pos == 6) {this.pos = 1} else {this.pos += 1}

  def samePiece(x: Piece) = {
   this.left == x.left && this.bottom == x.bottom && this.right == x.right
  }
  def convertPos = {
    this.pos match {
      case 1 => (left, right, 'O', bottom)  //(left, right, up, down)
      case 3 => (bottom, left, 'O', right)
      case 5 => (right, bottom, 'O', left)
      case 2 => (bottom, right, left, 'O')
      case 4 => (right, left, bottom, 'O')
      case 6 => (left, bottom, right, 'O')
    }
  }
}

