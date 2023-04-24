package puzzle.UI

import puzzle._

import scala.swing._
import scala.swing.event._
import javax.imageio.ImageIO
import java.io.File
import java.awt.{ Image, Color, Graphics2D , Toolkit }


object PuzzleGUI extends SimpleSwingApplication {

  // The dimensions of the game window.
  val screenDimension = Toolkit.getDefaultToolkit.getScreenSize
  val (width, height) = (Toolkit.getDefaultToolkit.getScreenSize.getWidth.toInt,
    Toolkit.getDefaultToolkit.getScreenSize.getHeight.toInt)
  val windowSize = new Dimension(width, height)

  // The game and the stack and the board of the game.
  val game = new Game
  val board = game.board
  val stack = game.pile

   // The main frame.
  def top: MainFrame = new MainFrame {
    title = "The Triangle Puzzle"
    minimumSize = new Dimension(width, height)
    maximumSize = new Dimension(width, height)
    peer.setLocationRelativeTo(null)
    contents = new MainPanel
  }

  // Define variables for mouse movement coordinates.
  var movedPiece: Option[Piece] = None
  var startCoords = (0, 0)
  var movedCoords = (0, 0)

  // Define the dimensions and coordinates for the board background.
  val (w1, h1, coords1) = (5 * width / 9, 3 * height / 4, (3 * width / 8, height / 8))

  // Define the dimensions and coordinates for the stack background.
  val (w2, coords2, h2) = (7 * width / 32, (3 * width / 32, height / 8), 3 * height / 4)

  // Define the coordinates for the corners of the board.
  val boardCorner1 = (coords1._1 + 50 , coords1._2 + h1 / 2)
  val boardCorner4 = (coords1._1 + w1 - 50, coords1._2 + h1 / 2)

  // Define the side and diameter of a triangle piece.
  val side = (boardCorner4._1 - boardCorner1._1) / 4.0
  val diameter = side * 1.0 / math.sqrt(2)

  val boardCorner2 = (boardCorner1._1 + side.toInt, boardCorner1._2 - (2 * diameter).toInt)
  val boardCorner3 = (boardCorner2._1 + (2 * side).toInt, boardCorner2._2)
  val boardCorner5 = (boardCorner3._1, boardCorner1._2 + (2 * diameter).toInt)
  val boardCorner6 = (boardCorner2._1, boardCorner5._2)
  val boardCorner12 = (boardCorner1._1 + ((boardCorner2._1 - boardCorner1._1) / 2.0).toInt, boardCorner2._2 + ((boardCorner1._2 - boardCorner2._2) / 2.0).toInt)
  val boardCorner23 = (boardCorner2._1 + side.toInt, boardCorner2._2)

   // The scaled images for the background of the triangle pieces and the board
  val bgorg = ImageIO.read(new File("/Users/egor/IdeaProjects/my-triangle-puzzle/main/src/resources/background.png"))
  val bgImg = bgorg.getScaledInstance(width, height, Image.SCALE_DEFAULT)
  val triangleorg = ImageIO.read(new File("/Users/egor/IdeaProjects/my-triangle-puzzle/main/src/resources/triangle.png"))
  val triangleUpImg = triangleorg.getScaledInstance(side.toInt, diameter.toInt, Image.SCALE_DEFAULT)
  val triangledownorg = ImageIO.read(new File("/Users/egor/IdeaProjects/my-triangle-puzzle/main/src/resources/triangledown.png"))
  val triangleDownImg = triangledownorg.getScaledInstance(side.toInt, diameter.toInt, Image.SCALE_DEFAULT)


  // Draws the background for the game and the game board and the pieces for the stack and the board as well as the moving piece.
  def onPaint(g: Graphics2D) = {
    g.drawImage(bgImg, 0, 0, null)
    g.setColor(new Color(255, 250, 250))
    g.fillRect(coords1._1, coords1._2, w1, h1)
    g.fillRect(coords2._1, coords2._2, w2, h2)

    // Draws the lines for the board.
    g.setColor(Color.black)
    g.drawLine(boardCorner12._1, boardCorner12._2, boardCorner12._1 + (3 * side).toInt, boardCorner12._2)
    g.drawLine(boardCorner12._1, boardCorner12._2 + (2 * diameter).toInt, boardCorner12._1 + (3 * side).toInt, boardCorner12._2 + (2 * diameter).toInt)
    g.drawLine(boardCorner12._1, boardCorner12._2, boardCorner23._1, boardCorner23._2 + (4 * diameter).toInt)
    g.drawLine(boardCorner23._1, boardCorner23._2, boardCorner12._1 + (3 * side).toInt, boardCorner12._2 + (2 * diameter).toInt)
    g.drawLine(boardCorner23._1, boardCorner23._2, boardCorner12._1, boardCorner12._2 + (2 * diameter).toInt)
    g.drawLine(boardCorner23._1, boardCorner23._2 + (4 * diameter).toInt, boardCorner12._1 + (3 * side).toInt, boardCorner12._2)
    g.drawLine(boardCorner1._1, boardCorner1._2, boardCorner2._1, boardCorner2._2)
    g.drawLine(boardCorner1._1, boardCorner1._2, boardCorner4._1, boardCorner4._2)
    g.drawLine(boardCorner1._1, boardCorner1._2, boardCorner6._1, boardCorner6._2)
    g.drawLine(boardCorner2._1, boardCorner2._2, boardCorner3._1, boardCorner3._2)
    g.drawLine(boardCorner2._1, boardCorner2._2, boardCorner5._1, boardCorner5._2)
    g.drawLine(boardCorner3._1, boardCorner3._2, boardCorner4._1, boardCorner4._2)
    g.drawLine(boardCorner3._1, boardCorner3._2, boardCorner6._1, boardCorner6._2)
    g.drawLine(boardCorner4._1, boardCorner4._2, boardCorner5._1, boardCorner5._2)
    g.drawLine(boardCorner5._1, boardCorner5._2, boardCorner6._1, boardCorner6._2)

    // Draws the piece on top of the stack and the symbols on its sides
    val currentPiece = stack.currentPiece.getOrElse(board.padPiece)
    val font2 = new Font("Arial", java.awt.Font.BOLD, w2 / 24)
    g.setColor(new Color(255, 250, 250))
    g.setFont(font2)
    if (!currentPiece.samePiece(board.padPiece)) {
      val symbols = currentPiece.convertPos
      val centerY = coords2._2 + (h2 / 2)
      val centerX = coords2._1 + (w2 / 5) + 5
      if (currentPiece.position%2 == 1) {
        g.drawImage(triangleUpImg, centerX, centerY, null)
        g.drawString(symbols._1.toString, centerX + (side / 4).toInt + 20, centerY+ (diameter / 2).toInt)  //left
        g.drawString(symbols._2.toString, centerX + (side * 3 / 4).toInt - 30, centerY+ (diameter / 2).toInt)  //right
        g.drawString(symbols._4.toString, centerX + (side / 2).toInt, centerY+ diameter.toInt - 15)  //bottom
      } else {
        g.drawImage(triangleDownImg, centerX, centerY, null)
        g.drawString(symbols._1.toString, centerX + (side / 4).toInt + 15, centerY+ (diameter / 2).toInt)  //left
        g.drawString(symbols._2.toString, centerX + (side * 3 / 4).toInt - 20, centerY+ (diameter / 2).toInt)  //right
        g.drawString(symbols._3.toString, centerX + (side / 2).toInt, centerY+ 25)  //top
      }
    }

    // Draws the pieces that are currently on the board
    for {
      i <- 0 until board.rows
      j <- 0 until board.columns
    } {
      val x11 = boardCorner2._1 - (side / 2).toInt
      val y11 = boardCorner2._2
      val x21 = boardCorner1._1
      val y21 = boardCorner1._2 - diameter.toInt
      val addedX = (side / 2)
      val addedY = diameter
      val piece = board.getBoard(i)(j).getOrElse(board.padPiece)

      if (!piece.samePiece(board.padPiece)) {
        val symbols = piece.convertPos
        if (piece.position%2 == 1) {
          if (i == 1 || i == 4) {
            g.drawImage(triangleUpImg, x11 + ((j - 2) * addedX).toInt, y11 + ((i - 1) * addedY).toInt, null)
            g.drawString(symbols._1.toString, x11 + ((j - 2) * addedX).toInt + (side / 4).toInt + 20, y11 + ((i - 1) * addedY).toInt + (diameter / 2).toInt)  //left
            g.drawString(symbols._2.toString, x11 + ((j - 2) * addedX).toInt + (side * 3 / 4).toInt - 30, y11 + ((i - 1) * addedY).toInt + (diameter / 2).toInt)  //right
            g.drawString(symbols._4.toString, x11 + ((j - 2) * addedX).toInt + (side / 2).toInt, y11 + ((i - 1) * addedY).toInt + diameter.toInt - 15)  //bottom
          }
          if (i == 2 || i == 3) {
            g.drawImage(triangleUpImg, x21 + ((j - 1) * addedX).toInt, y21 + ((i - 2) * addedY).toInt, null)
            g.drawString(symbols._1.toString, x21 + ((j - 1) * addedX).toInt + (side / 4).toInt + 20, y21 + ((i - 2) * addedY).toInt + (diameter / 2).toInt)  //left
            g.drawString(symbols._2.toString, x21 + ((j - 1) * addedX).toInt + (side * 3 / 4).toInt - 30, y21 + ((i - 2) * addedY).toInt + (diameter / 2).toInt)  //right
            g.drawString(symbols._4.toString, x21 + ((j - 1) * addedX).toInt + (side / 2).toInt, y21 + ((i - 2) * addedY).toInt + diameter.toInt - 15)  //bottom
          }
        } else {
          if (i == 1 || i == 4) {
            g.drawImage(triangleDownImg, x11 + ((j - 2) * addedX).toInt, y11 + ((i - 1) * addedY).toInt, null)
            g.drawString(symbols._1.toString, x11 + ((j - 2) * addedX).toInt + (side / 4).toInt + 15, y11 + ((i - 1) * addedY).toInt + (diameter / 2).toInt)  //left
            g.drawString(symbols._2.toString, x11 + ((j - 2) * addedX).toInt + (side * 3 / 4).toInt - 20, y11 + ((i - 1) * addedY).toInt + (diameter / 2).toInt)  //right
            g.drawString(symbols._3.toString, x11 + ((j - 2) * addedX).toInt + (side / 2).toInt, y11 + ((i - 1) * addedY).toInt + 25)  //top
          }
          if (i == 2 || i == 3) {
            g.drawImage(triangleDownImg, x21 + ((j - 1) * addedX).toInt, y21 + ((i - 2) * addedY).toInt, null)
            g.drawString(symbols._1.toString, x21 + ((j - 1) * addedX).toInt + (side / 4).toInt + 15, y21 + ((i - 2) * addedY).toInt + (diameter / 2).toInt)  //left
            g.drawString(symbols._2.toString, x21 + ((j - 1) * addedX).toInt + (side * 3 / 4).toInt - 20, y21 + ((i - 2) * addedY).toInt + (diameter / 2).toInt)  //right
            g.drawString(symbols._3.toString, x21 + ((j - 1) * addedX).toInt + (side / 2).toInt, y21 + ((i - 2) * addedY).toInt + 25)  //top
          }
        }
      }
    }

    // Draws the piece that is being moved
    val moved = movedPiece.getOrElse(board.padPiece)
    if (!moved.samePiece(board.padPiece)) {
      val symbols = moved.convertPos
      if (moved.position%2 == 1) {
        g.drawImage(triangleUpImg, movedCoords._1, movedCoords._2, null)
        g.drawString(symbols._1.toString, movedCoords._1 + (side / 4).toInt + 20, movedCoords._2 + (diameter / 2).toInt)  //left
        g.drawString(symbols._2.toString, movedCoords._1 + (side * 3 / 4).toInt - 30, movedCoords._2 + (diameter / 2).toInt)  //right
        g.drawString(symbols._4.toString, movedCoords._1 + (side / 2).toInt, movedCoords._2 + diameter.toInt - 15)  //bottom
      } else {
        g.drawImage(triangleDownImg, movedCoords._1, movedCoords._2, null)
        g.drawString(symbols._1.toString, movedCoords._1 + (side / 4).toInt + 15, movedCoords._2 + (diameter / 2).toInt)  //left
        g.drawString(symbols._2.toString, movedCoords._1 + (side * 3 / 4).toInt - 20, movedCoords._2 + (diameter / 2).toInt)  //right
        g.drawString(symbols._3.toString, movedCoords._1 + (side / 2).toInt, movedCoords._2 + 25)  //top
      }
    }

  }

  // Converts the coords on the screen given in pixels to match the indexes in the board array
  def convertCoords(x: Int, y: Int): (Int, Int) = {
    val x11 = boardCorner2._1 - (side / 2).toInt
    val y11 = boardCorner2._2
    val x11Side = x11 + (side / 4).toInt
    val y11Side = y11 + (diameter / 2).toInt
    val addedX = (side / 2)
    val addedY = (diameter / 2)

    if (y >= y11Side && y <= (y11Side + addedY.toInt))
        if (x >= x11Side && x <= (x11Side + addedX.toInt)) (1, 2) else //(y, x)
          if (x >= (x11Side + (2 * addedX).toInt) && x <= (x11Side + (3 * addedX).toInt)) (1, 4) else
            if (x >= (x11Side + (4 * addedX).toInt) && x <= (x11Side + (5 * addedX).toInt)) (1, 6) else (-1, -1)

    else if (y >= y11 && y <= y11Side)
        if (x >= (x11Side + addedX.toInt) && x <= (x11Side + (2 * addedX).toInt)) (1, 3) else
          if (x >= (x11Side + (3 * addedX).toInt) && x <= (x11Side + (4 * addedX).toInt)) (1, 5) else (-1, -1)

    else if (y >= (y11Side + (2 * addedY).toInt) && y <= (y11Side + (3 * addedY).toInt))
        if (x >= (x11Side - addedX.toInt) && x <= x11Side) (2, 1) else
          if (x >= (x11Side + addedX.toInt) && x <= (x11Side + (2 * addedX).toInt)) (2, 3) else
            if (x >= (x11Side + (3 * addedX).toInt) && x <= (x11Side + (4 * addedX).toInt)) (2, 5) else
              if (x >= (x11Side + (5 * addedX).toInt) && x <= (x11Side + (6 * addedX).toInt)) (2, 7) else (-1, -1)

    else if (y >= (y11Side + addedY.toInt) && y <= (y11Side + (2 * addedY).toInt))
        if (x >= x11Side && x <= (x11Side + addedX.toInt)) (2, 2) else
          if (x >= (x11Side + (2 * addedX).toInt) && x <= (x11Side + (3 * addedX).toInt)) (2, 4) else
            if (x >= (x11Side + (4 * addedX).toInt) && x <= (x11Side + (5 * addedX).toInt)) (2, 6) else  (-1, -1)

    else if (y >= (y11Side + (3 * addedY).toInt) && y <= (y11Side + (4 * addedY).toInt))
        if (x >= (x11Side - addedX.toInt) && x <= x11Side) (3, 1) else
          if (x >= (x11Side + addedX.toInt) && x <= (x11Side + (2 * addedX).toInt)) (3, 3) else
            if (x >= (x11Side + (3 * addedX).toInt) && x <= (x11Side + (4 * addedX).toInt)) (3, 5) else
              if (x >= (x11Side + (5 * addedX).toInt) && x <= (x11Side + (6 * addedX).toInt)) (3, 7) else (-1, -1)

    else if (y >= (y11Side + (4 * addedY).toInt) && y <= (y11Side + (5 * addedY).toInt))
        if (x >= x11Side && x <= (x11Side + addedX.toInt)) (3, 2) else
          if (x >= (x11Side + (2 * addedX).toInt) && x <= (x11Side + (3 * addedX).toInt)) (3, 4) else
            if (x >= (x11Side + (4 * addedX).toInt) && x <= (x11Side + (5 * addedX).toInt)) (3, 6) else (-1, -1)

    else if (y >= (y11Side + (5 * addedY).toInt) && y <= (y11Side + (6 * addedY).toInt))
        if (x >= x11Side && x <= (x11Side + addedX.toInt)) (4, 2) else
          if (x >= (x11Side + (2 * addedX).toInt) && x <= (x11Side + (3 * addedX).toInt)) (4, 4) else
            if (x >= (x11Side + (4 * addedX).toInt) && x <= (x11Side + (5 * addedX).toInt)) (4, 6) else (-1, -1)

    else if (y >= (y11Side + (6 * addedY).toInt) && y <= (y11Side + (7 * addedY).toInt))
        if (x >= (x11Side + addedX.toInt) && x <= (x11Side + (2 * addedX).toInt)) (4, 3) else
          if (x >= (x11Side + (3 * addedX).toInt) && x <= (x11Side + (4 * addedX).toInt)) (4, 5) else (-1, -1)

    else (-1, -1)
  }

  // Called when mouse is clicked, if this occurs on the stack or on a piece on the board, the piece is rotated
  def onPress(x: Int, y: Int) = {
    val stackY = coords2._2 + (h2 / 2)
    val stackX = coords2._1 + (w2 / 5) + 5

    // Rotate the piece in the stack if the click occurs within the bounds of the stack
    if ((x >= stackX && x <= (stackX + side)) && (y >= stackY && y <= (stackY + diameter))) {
      val current = stack.currentPiece.getOrElse(board.padPiece)
      if (!current.samePiece(board.padPiece)) {
        current.rotate()
      }
    }

    // Rotate the piece on the board if the click occurs within the bounds of a piece on the board
    val converted = convertCoords(x, y)
    if (converted._1 != -1) {
      val current = board.getBoard(converted._1)(converted._2).getOrElse(board.padPiece)
      if (!current.samePiece(board.padPiece)) {
        current.rotate()
        current.rotate()
      }
    }
  }

  // Called when a mouse is movedUpdates the value of movedPiece, the piece that is currently moved
  def onMove(x: Int, y: Int) = {
    val stackY = coords2._2 + (h2 / 2)
    val stackX = coords2._1 + (w2 / 5) + 5

    // Check if the moved piece is from the stack.
    if ((startCoords._1 >= stackX && startCoords._1 <= (stackX + side)) && (startCoords._2 >= stackY && startCoords._2 <= (stackY + diameter))) {
      val current = stack.currentPiece.getOrElse(board.padPiece)
      if (!current.samePiece(board.padPiece) && movedPiece.isEmpty) {
        stack.takePiece(current)
        movedPiece = Some(current)
      }

    // Check if the moved piece is from the board
    } else {
      val convertedStart = convertCoords(startCoords._1, startCoords._2)
      if (convertedStart._1 != -1) {
        val current = board.getBoard(convertedStart._1)(convertedStart._2).getOrElse(board.padPiece)
        if (!current.samePiece(board.padPiece) && movedPiece.isEmpty) {
          board.removePiece(current)
          movedPiece = Some(current)
        }
      }
    }
  }

  // Called when the mouse is released
  def onRelease(x: Int, y: Int): Unit = {
    val stackY = coords2._2 + (h2 / 2)
    val stackX = coords2._1 + (w2 / 5) + 5
    val current = movedPiece.getOrElse(board.padPiece)
    val converted = convertCoords(x, y)

    // The start coords are on top of the stack, if the end coords are wrong, the piece is added back to the stack
    if ((startCoords._1 >= stackX && startCoords._1 <= (stackX + side)) && (startCoords._2 >= stackY && startCoords._2 <= (stackY + diameter))) {
      if (converted._1 != -1) {
        if (!current.samePiece(board.padPiece) && movedPiece.isDefined) {
          if (converted._1 == 1 || converted._1 == 4) {
            val success = board.addPiece(current, converted._2 - 1, converted._1)
            if (!success) { stack.addPiece(current) }
          }
          if (converted._1 == 2 || converted._1 == 3) {
            val success = board.addPiece(current, converted._2, converted._1)
            if (!success) stack.addPiece(current)
          }
        }
      } else {
        if (!current.samePiece(board.padPiece)) { stack.addPiece(current) }
      }

    // The start coords are on top of the stack. If the end coords are wrong, the piece is added back to the stack.
    } else {
      val convertedStart = convertCoords(startCoords._1, startCoords._2)
      if (convertedStart._1 != -1) {
        if (converted._1 != -1) {
          if (!current.samePiece(board.padPiece) && movedPiece.isDefined) {
            if (converted._1 == 1 || converted._1 == 4) {
              val success = board.addPiece(current, converted._2 - 1, converted._1)
              if (!success) {
                if (convertedStart._1 == 1 || convertedStart._1 == 4) { board.addPiece(current, convertedStart._2 - 1, convertedStart._1) }
                if (convertedStart._1 == 2 || convertedStart._1 == 3) { board.addPiece(current, convertedStart._2, convertedStart._1) }
              }
            }
            if (converted._1 == 2 || converted._1 == 3) {
              val success = board.addPiece(current, converted._2, converted._1)
              if (!success) {
                if (convertedStart._1 == 1 || convertedStart._1 == 4) { board.addPiece(current, convertedStart._2 - 1, convertedStart._1) }
                if (convertedStart._1 == 2 || convertedStart._1 == 3) { board.addPiece(current, convertedStart._2, convertedStart._1) }
              }
            }
          }
        } else {
          if ((x >= stackX && x <= (stackX + side)) && (y >= stackY && y <= (stackY + diameter))) {
            if (current.left != 'x') { stack.addPiece(current) }
          } else {
            if (!current.samePiece(board.padPiece)) {
              if (convertedStart._1 == 1 || convertedStart._1 == 4) { board.addPiece(current, convertedStart._2 - 1, convertedStart._1) }
              if (convertedStart._1 == 2 || convertedStart._1 == 3) { board.addPiece(current, convertedStart._2, convertedStart._1) }
            }
          }
        }
      }
    }
  }

  // The class for the main panel, it contains all of the buttons and listens to them and the mouse
  class MainPanel extends BoxPanel(Orientation.Vertical) {

    // Draws the image
    override def paintComponent(g: Graphics2D) = {
      onPaint(g)
    }

    val button1Dim = new Dimension(w2 / 3, h2 / 16)
    val button2Dim = new Dimension(w2 / 5, h2 / 16)
    val buttonFont = new Font("Arial", java.awt.Font.PLAIN, w2 / 28)

    // The buttons
    val newGame = new Button {
      text = "New game"
      font = buttonFont
      minimumSize = button1Dim
      preferredSize = button1Dim
      maximumSize = button1Dim
    }
    val continue = new Button {
      font = buttonFont
      text = "Continue game"
      minimumSize = button1Dim
      preferredSize = button1Dim
      maximumSize = button1Dim
    }
    val save = new Button {
      font = buttonFont
      text = "Save and close"
      minimumSize = button1Dim
      preferredSize = button1Dim
      maximumSize = button1Dim
    }
    val next = new Button {
      text = ">"
      minimumSize = button2Dim
      preferredSize = button2Dim
      maximumSize = button2Dim
    }
    val previous = new Button {
      text = "<"
      minimumSize = button2Dim
      preferredSize = button2Dim
      maximumSize = button2Dim
    }

    // Creates the structure of the main frame by combining panels
    val buttons11 = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(3 * width / 32 + 3 * width / 100)
      contents += newGame
      contents += Swing.HStrut(2 * width / 100)
      contents += continue
      background = new Color(0, 0, 0, 0)
    }
    val buttons12 = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(3 * width / 32 + 3 * width / 100)
      contents += Swing.HStrut(2 * width / 100)
      contents += save
      background = new Color(0, 0, 0, 0)
    }
    val buttons1 = new BoxPanel(Orientation.Vertical) {
      contents += buttons11
      contents += Swing.VStrut(-1 * height / 3 + 3 * height / 100)
      contents += buttons12
      background = new Color(0, 0, 0, 0)
    }
    val buttons2 = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(4 * width / 32 + 2 * width / 100)
      contents += previous
      contents += Swing.HStrut(3 * width / 100)
      contents += next
      background = new Color(0, 0, 0, 0)
    }

    // Adds the panels to the contents of the main panel
    contents += new BorderPanel {
      add(buttons1, BorderPanel.Position.West)
      background = new Color(0, 0, 0, 0)
    }
    contents += Swing.VStrut(10)
    contents += Swing.VStrut(10)
    contents += new BorderPanel {
      add(buttons2, BorderPanel.Position.West)
      background = new Color(0, 0, 0, 0)
    }

    // Pop-up windows
    def startMessage(): Unit = {
      val text = "Welcome to the Triangle Puzzle! You started a new gameThese are the rules: \n" +
      "You must solve the puzzle by dragging and matching the sides of pieces: uppercase to lowercase and vice versa \n" +
      "You can click on the arrows to flip through the stack of pieces and click on the piece on top to rotate it\n" +
      "You can save your game and continue it later by pressing on the 'Save and close' buttonWhen you then wish to continue the game \n" +
      "open the app again and press on 'Continue game'\n"

      Dialog.showMessage(contents(1), text, title="Game started")
    }
    def continueMessage(): Unit = {
      val text = "The last saved game was continued"
      Dialog.showMessage(contents(1), text, title="Game continued")
    }

    def saveMessage(): Unit = {
      val text = "The game was successfully saved\n Do you want to close the app?"
      val res = Dialog.showConfirmation(contents(1), text, optionType=Dialog.Options.YesNo, title="Game saved")
      if (res == Dialog.Result.Yes) { sys.exit(0) }
    }
    def endMessage(): Unit = {
      val text = "Congratulations! You solved the puzzle successfully!"
      Dialog.showMessage(contents(1), text, title="Puzzle solved")
      game.endGame()
      repaint()
    }

    // Listens to the buttons and the mouseAt first, only listens to the buttons new game and continue
    listenTo(newGame)
    listenTo(continue)

    // Reacts to the buttons and mouse
    reactions += {
      case ButtonClicked(`newGame`) =>
        game.startGame()
        listenTo(save)
        listenTo(next)
        listenTo(previous)
        listenTo(mouse.clicks)
        listenTo(mouse.moves)
        deafTo(newGame)
        deafTo(continue)
        repaint()
        startMessage()
      case ButtonClicked(`continue`) =>
        val gameSituation = FileOperations.readFromFile("gameFile.txt")
        game.continueGame(gameSituation._1, gameSituation._2, gameSituation._3)
        listenTo(save)
        listenTo(next)
        listenTo(previous)
        listenTo(mouse.clicks)
        listenTo(mouse.moves)
        deafTo(newGame)
        deafTo(continue)
        repaint()
        continueMessage()
      case ButtonClicked(`save`) =>
        FileOperations.writeToFile("gameFile.txt", board.getBoard, stack.pieceStack, game.solution)
        saveMessage()
      case ButtonClicked(`next`) =>
        stack.flipRight()
        repaint()
      case ButtonClicked(`previous`) =>
        stack.flipLeft()
        repaint()
      case MouseClicked(_, p, _, _, _) =>
        onPress(p.x, p.y)
        repaint()
        if (game.gameOver) {
          listenTo(newGame)
          listenTo(continue)
          deafTo(save)
          deafTo(next)
          deafTo(previous)
          deafTo(mouse.clicks)
          deafTo(mouse.moves)
          endMessage()
        }
      case MousePressed(_, p, _, _, _) =>
        startCoords = (p.x, p.y)
      case MouseDragged(_, p, _) =>
        onMove(p.x, p.y)
        movedCoords = (p.x - (side / 2).toInt, p.y - (diameter / 2).toInt)
        repaint()
      case MouseReleased(_, p, _, _, _) =>
        onRelease(p.x, p.y)
        repaint()
        movedPiece = None
        movedCoords = (0, 0)
        startCoords = (0, 0)
        if (game.gameOver) {
          listenTo(newGame)
          listenTo(continue)
          deafTo(save)
          deafTo(next)
          deafTo(previous)
          deafTo(mouse.clicks)
          deafTo(mouse.moves)
          endMessage()
        }
    }
  }

}