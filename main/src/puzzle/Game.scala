package puzzle

import scala.collection.mutable.Buffer
import scala.util.Random

class Game {
  val board = new Board
  val stack = new PiecesStack

  private val piecePlaces = board.placesOnBoard

  private var sol = Array.ofDim[Option[Piece]](6, 9)

  private var gameStarted = false

  def solution = this.sol

  // Fills solution with padding pieces
  private def solInit() = {
    for {
      rowIndex <- this.sol.indices
      colIndex <- this.sol(rowIndex).indices
    } {
      if (this.piecePlaces(rowIndex)(colIndex) == 2 || (rowIndex == 5 && (colIndex == 3 || colIndex == 5))) {
        this.sol(rowIndex)(colIndex) = Some(new Piece('x', 'x', 'x', 2))
      } else {
        this.sol(rowIndex)(colIndex) = Some(new Piece('x', 'x', 'x', 1))
      }
    }
  }
    // Helper method: matching symbols
  private def matchingSymbol(c: Char): Char = {
    val symbols = Vector('A', 'B', 'C', 'D', 'a', 'b', 'c', 'd')
    c match {
      case 'A' => 'a'
      case 'B' => 'b'
      case 'C' => 'c'
      case 'D' => 'd'
      case 'a' => 'A'
      case 'b' => 'B'
      case 'c' => 'C'
      case 'd' => 'D'
      case 'O' => 'O'
      case 'x' => symbols(Random.nextInt(8))
    }
  }
  //A helper method that generates a new piece
  private def generatePiece(pieceOnLeft: Piece, pieceOnRight: Piece, pieceAbove: Piece, pieceBeneath: Piece, n: Int): Piece = {
      val a = this.matchingSymbol(pieceOnLeft.convertPos._2)
      val b = this.matchingSymbol(pieceOnRight.convertPos._1)
     if (n == 1) {
      val c = this.matchingSymbol(pieceBeneath.convertPos._3)
      new Piece(a, b, c, 1)
    } else {
      val c = this.matchingSymbol(pieceAbove.convertPos._4)
      new Piece(c, b, a, 2)
    }
  }

  private def generateSolution(): Unit = {
    var existingPieces: Buffer[Piece] = Buffer[Piece]()
    this.solInit()

    def pieceExists(piece: Piece): Boolean = {
      existingPieces.exists(_.samePiece(piece))
  }

  for {
    rowIndex <- this.sol.indices
    colIndex <- this.sol(rowIndex).indices
  } {
    var piece = this.board.padPiece
    do {
      if (this.piecePlaces(rowIndex)(colIndex) == 1) {
        piece = this.generatePiece(
          this.sol(rowIndex)(colIndex - 1).get,
          this.sol(rowIndex)(colIndex + 1).get,
          this.sol(rowIndex - 1)(colIndex).get,
          this.sol(rowIndex + 1)(colIndex).get,
          1
        )
      }
      if (this.piecePlaces(rowIndex)(colIndex) == 2) {
        piece = this.generatePiece(
          this.sol(rowIndex)(colIndex - 1).get,
          this.sol(rowIndex)(colIndex + 1).get,
          this.sol(rowIndex - 1)(colIndex).get,
          this.sol(rowIndex + 1)(colIndex).get,
          2
        )
      }
    } while (pieceExists(piece))

    if (this.piecePlaces(rowIndex)(colIndex) != 0) {
      this.sol(rowIndex)(colIndex) = Some(piece)
      if (rowIndex == 1 || rowIndex == 4) {
        piece.addCoords(rowIndex, colIndex - 1)
      } else {
        piece.addCoords(rowIndex, colIndex)
      }
      existingPieces += new Piece(piece.left, piece.right, piece.bottom, piece.position)
    }
  }

  for (_ <- 1 to 24) {
    val shuffledPieces = Random.shuffle(existingPieces)
    shuffledPieces.foreach(_.rotate())
    this.stack.addPiece(shuffledPieces.head)
    existingPieces = shuffledPieces.tail
  }
}
  //starts the game
  def startGame() = {
    if (!this.gameStarted) {
      this.generateSolution()
      this.gameStarted = true
    }
  }
  // Continues the game
  def continueGame(situation: Array[Array[Option[Piece]]], stackSituation: Buffer[Piece], solution: Array[Array[Option[Piece]]]): Unit = {
    if (!gameStarted) {
  val boardArr = situation
  for {
    rowIndex <- boardArr.indices
    colIndex <- boardArr(rowIndex).indices
  } {
    val p = boardArr(rowIndex)(colIndex).getOrElse(board.padPiece)
    if (!p.samePiece(board.padPiece)) {
      if (rowIndex == 1 || rowIndex == 4) {
        board.addPiece(p, colIndex - 1, rowIndex)
      } else {
        board.addPiece(p, colIndex, rowIndex)
      }
    }
  }
  // Adds the pieces to the pile
  val stackBuf = stackSituation
  for (p <- stackBuf.indices) {
    stack.addPiece(stackBuf(p))
  }
  // Updates the solution to match the solution of the continued game
  sol = solution
  for {
    rowIndex <- sol.indices
    colIndex <- sol(rowIndex).indices
  } {
    if (piecePlaces(rowIndex)(colIndex) != 0) {
      val p = sol(rowIndex)(colIndex)
      if ((rowIndex == 1 || rowIndex == 4) && p.isDefined) {
        p.get.addCoords(rowIndex, colIndex - 1)
      } else {
        if (p.isDefined) {
          p.get.addCoords(rowIndex, colIndex)
        }
      }
    }
  }
  if (board.isEmpty && stack.isEmpty) {
    generateSolution()
  }
  gameStarted = true
}
  }
    // Checks that a given piece has the correct symbols on its sides
  private def correctSides(p: Piece, l: Char, u: Char): Boolean = {
    val converted = p.convertPos
    (this.matchingSymbol(l) == converted._1 || l == 'x') && (this.matchingSymbol(u) == converted._3 || u == 'x')
  }

   // Checks whether a solution is found on the board
  private def solutionFound: Boolean = {
    if (this.board.isFull) {
      val solFound = Buffer[Boolean]()
      for {
        rowIndex <- 0 until board.rows
        colIndex <- 0 until board.columns
      } {
        val p = this.board.getBoard(rowIndex)(colIndex).get
        if (!p.samePiece(this.board.padPiece)) {
          val l = this.board.getBoard(rowIndex)(colIndex - 1).get.convertPos._2
          val u = this.board.getBoard(rowIndex - 1)(colIndex).get.convertPos._4
          solFound += this.correctSides(p, l, u)
        }
      }
      solFound.forall( _ == true )
    } else false
  }


  def gameOver = this.gameStarted && this.solutionFound

  def endGame() = {
    if (this.gameOver) {
      this.sol = Array.ofDim[Option[Piece]](6, 9)
      this.board.empty()
      this.stack.empty()
      this.gameStarted = false
    }
  }
}