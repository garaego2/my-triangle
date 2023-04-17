package puzzle

import scala.collection.mutable.Buffer
import scala.util.Random

class Game {
  val board = new Board
  val pile = new PiecesStack

  val piecePlaces = board.placesOnBoard

  private var sol = Array.ofDim[Option[Piece]](6, 9)

  private var gameStarted = false

  def solution = this.sol

  private def solInit() = {
    for {
      i <- this.sol.indices
      j <- this.sol(i).indices
    } {
      if (this.piecePlaces(i)(j) == 2 || (i == 5 && (j == 3 || j == 5))) {
        this.sol(i)(j) = Some(new Piece('x', 'x', 'x', 2))
      } else {
        this.sol(i)(j) = Some(new Piece('x', 'x', 'x', 1))
      }
    }
  }

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

  private def generatePiece(pieceOnLeft: Piece, pieceOnRight: Piece, pieceAbove: Piece, pieceBeneath: Piece, n: Int): Piece = {
    if (n == 1) {
      val a = this.matchingSymbol(pieceOnLeft.convertPos._2)
      val b = this.matchingSymbol(pieceOnRight.convertPos._1)
      val c = this.matchingSymbol(pieceBeneath.convertPos._3)
      new Piece(a, b, c, 1)
    } else {
      val a = this.matchingSymbol(pieceOnLeft.convertPos._2)
      val b = this.matchingSymbol(pieceOnRight.convertPos._1)
      val c = this.matchingSymbol(pieceAbove.convertPos._4)
      new Piece(c, b, a, 2)
    }
  }

  private def generateSolution() = {

    var existingPieces = Buffer[Piece]()

    this.solInit()

    def pieceExists(p: Piece) = {
      existingPieces.exists( _.samePiece(p) )
    }

    for {
      i <- this.sol.indices
      j <- this.sol(i).indices
    } {
      var piece = this.board.padPiece
      do {
        if (this.piecePlaces(i)(j) == 1) {
          piece = this.generatePiece(this.sol(i)(j - 1).get, this.sol(i)(j + 1).get, this.sol(i - 1)(j).get, this.sol(i + 1)(j).get, 1)
        }
        if (this.piecePlaces(i)(j) == 2) {
          piece = this.generatePiece(this.sol(i)(j - 1).get, this.sol(i)(j + 1).get, this.sol(i - 1)(j).get, this.sol(i + 1)(j).get, 2)
        }
      } while (pieceExists(piece))

      if (this.piecePlaces(i)(j) != 0) {
        this.sol(i)(j) = Some(piece)
        if (i == 1 || i == 4) {
          piece.addCoords(i, j - 1)
        } else {
          piece.addCoords(i, j)
        }
        existingPieces += new Piece(piece.left, piece.right, piece.bottom, piece.position)
      }

    }

    for (i <- 1 to 24) {
      val shuffled = Random.shuffle(existingPieces)
      shuffled.foreach( _.rotate() )
      this.pile.addPiece(shuffled.head)
      existingPieces = shuffled.tail
    }
  }

  def startGame() = {
    if (!this.gameStarted) {
      this.generateSolution()
      this.gameStarted = true
    }
  }

  def continueGame(situation: Array[Array[Option[Piece]]], pileSituation: Buffer[Piece], solution: Array[Array[Option[Piece]]]) = {
    if (!this.gameStarted) {

      val boardArr = situation
      for {
        i <- boardArr.indices
        j <- boardArr(i).indices
      } {
        val p = boardArr(i)(j).getOrElse(this.board.padPiece)
        if (!p.samePiece(this.board.padPiece)) {
          if (i == 1 || i == 4) {
            this.board.addPiece(p, j - 1, i)
          } else {
            this.board.addPiece(p, j, i)
          }
        }
      }


      val pileBuf = pileSituation
      for (p <- pileBuf.indices) {
        this.pile.addPiece(pileBuf(p))
      }


      sol = solution
      for {
        i <- this.sol.indices
        j <- this.sol(i).indices
      } {
        if (this.piecePlaces(i)(j) != 0) {
          val p = this.sol(i)(j)
          if ((i == 1 || i == 4) && p.isDefined) { p.get.addCoords(i, j - 1) }
          else {
            if (p.isDefined) {
              p.get.addCoords(i, j)
            }
          }
        }
      }

      if (this.board.isEmpty && this.pile.isEmpty) this.generateSolution
      this.gameStarted = true
    }
  }

  def getHelp() = {

    def addCorrectPiece(piece: Piece) = {
          for {
            i <- this.sol.indices
            j <- this.sol(i).indices
          } {
            val solPiece = this.sol(i)(j).get
            if (solPiece.samePiece(piece)) {
              val coords = solPiece.getCoords
              val pieceOnBoard = this.board.pieceOnCoords(coords._2, coords._1)
              if (pieceOnBoard.isDefined) {
                this.board.removePiece(pieceOnBoard.get)
                this.pile.addPiece(pieceOnBoard.get)
              }
              this.board.addPiece(piece, coords._2, coords._1)
              this.pile.takePiece(piece)
              while (piece.position != solPiece.position) { piece.rotate() }
            }
          }
    }


    if (this.gameStarted) {


      for {
        i <- this.sol.indices
        j <- this.sol(i).indices
      } {
        val solPiece = this.sol(i)(j).get
        val boardPiece = this.board.getBoard(i)(j)
        if (boardPiece.isDefined) {
          val piece = boardPiece.get
          if (piece.samePiece(solPiece)) { while (piece.position != solPiece.position) { piece.rotate() } }
          else {
            this.board.removePiece(piece)
            this.pile.addPiece(piece)
          }
        }
      }


      if (this.pile.size >= 3) {
        for (n <- 1 to 3) {
          val piece = this.pile.piecePile.head
          addCorrectPiece(piece)
        }
      } else {

        if (this.pile.size >= 1) {
          val piece = this.pile.piecePile.head
          addCorrectPiece(piece)
        }
      }
    }
  }

  def correctSides(p: Piece, l: Char, u: Char): Boolean = {
    val converted = p.convertPos
    (this.matchingSymbol(l) == converted._1 || l == 'x') && (this.matchingSymbol(u) == converted._3 || u == 'x')
  }


  private def solutionFound: Boolean = {


    if (this.board.isFull) {
      val solFound = Buffer[Boolean]()
      for {
        i <- 0 until this.board.rows
        j <- 0 until this.board.columns
      } {
        val p = this.board.getBoard(i)(j).get
        if (!p.samePiece(this.board.padPiece)) {
          val l = this.board.getBoard(i)(j - 1).get.convertPos._2
          val u = this.board.getBoard(i - 1)(j).get.convertPos._4
          solFound += this.correctSides(p, l, u)
        }
      }
      solFound.forall( _ == true )
    } else { false }
  }


  def gameOver = this.gameStarted && this.solutionFound

  def endGame() = {
    if (this.gameOver) {
      this.sol = Array.ofDim[Option[Piece]](6, 9)
      this.board.empty()
      this.pile.empty()
      this.gameStarted = false
    }
  }

  private def correctPiece(piece: Piece, leftSymbol: Char, upSymbol: Char, p: Int, posN: Int): Boolean = {


    def nRotates(i: Int) = {
      for (j <- 0 until i) {
        piece.rotate()
      }
    }


    if (p == posN%2) {
      if (this.correctSides(piece, leftSymbol, upSymbol)) { true }
      else {
        nRotates(2)
        if (this.correctSides(piece, leftSymbol, upSymbol)) { true }
        else {
          nRotates(2)
          if (this.correctSides(piece, leftSymbol, upSymbol)) { true }
          else { false }
        }
      }


    } else {
      nRotates(1)
      if (this.correctSides(piece, leftSymbol, upSymbol)) { true }
      else {
        nRotates(2)
        if (this.correctSides(piece, leftSymbol, upSymbol)) { true }
        else {
          nRotates(2)
          if (this.correctSides(piece, leftSymbol, upSymbol)) { true }
          else { false }
        }
      }
    }
  }

  private def toCorrectPos(piece: Piece, leftSymbol: Char, upSymbol: Char, p: Int): Boolean = {
    piece.position match {
      case 1 => this.correctPiece(piece, leftSymbol, upSymbol, p, 1)
      case 2 => this.correctPiece(piece, leftSymbol, upSymbol, p, 2)
      case 3 => this.correctPiece(piece, leftSymbol, upSymbol, p, 3)
      case 4 => this.correctPiece(piece, leftSymbol, upSymbol, p, 4)
      case 5 => this.correctPiece(piece, leftSymbol, upSymbol, p, 5)
      case 6 => this.correctPiece(piece, leftSymbol, upSymbol, p, 6)
    }
  }


  def solveGame() = {


    var pieceStack = Buffer[(Piece, Int)]()


    var usedIndices = Buffer[Int]()

    if (!this.board.isEmpty) {
      for {
        i <- 0 until this.board.rows
        j <- 0 until this.board.columns
      } {
        val pieceOnBoard = this.board.getBoard(i)(j).getOrElse(this.board.padPiece)
        if (!pieceOnBoard.samePiece(this.board.padPiece)) {
          this.pile.addPiece(pieceOnBoard)
          this.board.removePiece(pieceOnBoard)
        }
      }
    }


    def valuesForSymbols(i: Int) = {
      val leftS = if (i == 0 || i == 5 || i == 12 || i == 19) { 'x' } else { pieceStack(i - 1)._1.convertPos._2 }
      val upS = if (i <= 5 || i == 11) { 'x' }
        else {
          if ((i >= 6 && i <= 10) || (i >= 19 && i <= 23)) { pieceStack(i - 6)._1.convertPos._4 }
          else { pieceStack(i - 7)._1.convertPos._4}
        }

      (leftS, upS)
    }


    def valueForPos(i: Int) = {
      if (i == 0 || i == 2 || i == 4 || i == 20 || i == 22 || (i >= 5 && i <= 18 && i%2 == 1)) 1 else 0
    }

    var symbolL = 'x'
    var symbolU = 'x'
    var position = 1
    var stackIndex = 0
    var pileIndex = 0
    var current = pile.pieceOnIndex(pileIndex)
    var last = 23

    while (pieceStack.size < 24) {
      if (!usedIndices.contains(pileIndex)) {
        val success = this.toCorrectPos(current.get, symbolL, symbolU, position)

        if (success) {
          pieceStack += ((current.get, pileIndex))
          usedIndices += pileIndex
          if (pileIndex == last) {
            do {
              last -= 1
            } while(usedIndices.contains(last))
          }
          stackIndex += 1
          position = valueForPos(stackIndex)
          symbolL = valuesForSymbols(stackIndex)._1
          symbolU = valuesForSymbols(stackIndex)._2
          pileIndex = 0
          current = this.pile.pieceOnIndex(pileIndex)
        } else {

          if (pileIndex == last) {
            var popped = pieceStack.last
            do {
              popped = pieceStack.last
              pieceStack = pieceStack.dropRight(1)
              usedIndices = usedIndices.dropRight(1)
              stackIndex -= 1
              if (popped._2 > last) { last = popped._2 }
            } while (popped._2 >= last)
            position = valueForPos(stackIndex)
            symbolL = valuesForSymbols(stackIndex)._1
            symbolU = valuesForSymbols(stackIndex)._2
            pileIndex = popped._2 + 1
            current = this.pile.pieceOnIndex(pileIndex)


          } else {
            pileIndex += 1
            current = this.pile.pieceOnIndex(pileIndex)
          }
        }


      } else {
        pileIndex += 1
        current = this.pile.pieceOnIndex(pileIndex)
      }

    }


    for (i <- pieceStack.indices) {
      if (i <= 4) { this.board.addPiece(pieceStack(i)._1, i + 1, 1) }
      if (i >= 5 && i <= 11) { this.board.addPiece(pieceStack(i)._1, i - 4, 2) }
      if (i >= 12 && i <= 18) { this.board.addPiece(pieceStack(i)._1, i - 11, 3) }
      if (i >= 19) { this.board.addPiece(pieceStack(i)._1, i - 18, 4) }
      this.pile.takePiece(pieceStack(i)._1)
    }

  }

}