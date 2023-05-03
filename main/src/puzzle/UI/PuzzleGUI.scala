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
  private val (width, height) = (Toolkit.getDefaultToolkit.getScreenSize.getWidth.toInt,
    Toolkit.getDefaultToolkit.getScreenSize.getHeight.toInt)
  val windowSize = new Dimension(width, height)

  // The game and the stack and the board of the game.
  private val game = new Game
  val board = game.board
  val stack = game.stack

   // The main frame.
  def top: MainFrame = new MainFrame {
    title = "The Triangle Puzzle"
    minimumSize = new Dimension(width, height)
    maximumSize = new Dimension(width, height)
    peer.setLocationRelativeTo(null)
    contents = new MainPanel
  }

  // Define variables for mouse movement coordinates.
  private var movedPiece: Option[Piece] = None
  private var startCoords = (0, 0)
  private var movedCoords = (0, 0)

  // Define the dimensions and coordinates for the board background.
  private val (w1, h1, coords1) = (5 * width / 9, 3 * height / 4, (3 * width / 8, height / 8))

  // Define the dimensions and coordinates for the stack background.
  private val (w2, coords2, h2) = (7 * width / 32, (3 * width / 32, height / 8), 3 * height / 4)

  // Define the coordinates for the corners of the board.
  private val boardCorner1 = (coords1._1 + 50 , coords1._2 + h1 / 2)
  private val boardCorner4 = (coords1._1 + w1 - 50, coords1._2 + h1 / 2)

  // Define the side and diameter of a triangle piece.
  private val side = (boardCorner4._1 - boardCorner1._1) / 4.0
  private val diameter = side * 1.0 / math.sqrt(2)

  private val boardCorner2 = (boardCorner1._1 + side.toInt, boardCorner1._2 - (2 * diameter).toInt)
  private val boardCorner3 = (boardCorner2._1 + (2 * side).toInt, boardCorner2._2)
  private val boardCorner5 = (boardCorner3._1, boardCorner1._2 + (2 * diameter).toInt)
  private val boardCorner6 = (boardCorner2._1, boardCorner5._2)
  private val boardCorner12 = (boardCorner1._1 + ((boardCorner2._1 - boardCorner1._1) / 2.0).toInt, boardCorner2._2 + ((boardCorner1._2 - boardCorner2._2) / 2.0).toInt)
  private val boardCorner23 = (boardCorner2._1 + side.toInt, boardCorner2._2)

   // The scaled images for the background of the triangle pieces and the board
  private val bgorg = ImageIO.read(new File("/Users/egor/IdeaProjects/my-triangle-puzzle/main/src/resources/background.png"))
  private val bgImg = bgorg.getScaledInstance(width, height, Image.SCALE_DEFAULT)
  private val triangleorg = ImageIO.read(new File("/Users/egor/IdeaProjects/my-triangle-puzzle/main/src/resources/triangle.png"))
  private val triangleUpImg = triangleorg.getScaledInstance(side.toInt, diameter.toInt, Image.SCALE_DEFAULT)
  private val triangledownorg = ImageIO.read(new File("/Users/egor/IdeaProjects/my-triangle-puzzle/main/src/resources/triangledown.png"))
  private val triangleDownImg = triangledownorg.getScaledInstance(side.toInt, diameter.toInt, Image.SCALE_DEFAULT)


  // Draws the background for the game, the game board, the pieces for the stack, the board as well as the moving piece.
   private def onPaint(g: Graphics2D) = {
    g.drawImage(bgImg, 0, 0, null)
    g.setColor(new Color(255, 250, 250))
    g.fillRect(coords1._1, coords1._2, w1, h1)
    g.fillRect(coords2._1, coords2._2, w2, h2)

    // Draws the lines for the board.
    val (x1, y1) = boardCorner12
    val (x2, y2) = boardCorner23
    val (x3, y3) = boardCorner1
    val (x4, y4) = boardCorner2
    val (x5, y5) = boardCorner3
    val (x6, y6) = boardCorner4
    val (x7, y7) = boardCorner5
    val (x8, y8) = boardCorner6

    g.setColor(Color.black)
    g.drawLine(x1, y1, x1 + (3 * side).toInt, y1)
    g.drawLine(x1, y1 + (2 * diameter).toInt, x1 + (3 * side).toInt, y1 + (2 * diameter).toInt)
    g.drawLine(x1, y1, x2, y2 + (4 * diameter).toInt)
    g.drawLine(x2, y2, x1 + (3 * side).toInt, y1 + (2 * diameter).toInt)
    g.drawLine(x2, y2, x1, y1 + (2 * diameter).toInt)
    g.drawLine(x2, y2 + (4 * diameter).toInt, x1 + (3 * side).toInt, y1)

    g.drawLine(x3, y3, x4, y4)
    g.drawLine(x3, y3, x6, y6)
    g.drawLine(x3, y3, x8, y8)
    g.drawLine(x4, y4, x5, y5)
    g.drawLine(x4, y4, x7, y7)
    g.drawLine(x5, y5, x6, y6)
    g.drawLine(x5, y5, x8, y8)
    g.drawLine(x6, y6, x7, y7)
    g.drawLine(x7, y7, x8, y8)


    // Draws the piece on top of the stack and the symbols on its sides
    val currentPiece = stack.currentPiece.getOrElse(board.padPiece)
    val font2 = new Font("Times New Roman", java.awt.Font.BOLD, w2 / 24)
    g.setColor(new Color(255, 250, 250))
    g.setFont(font2)

    if (!currentPiece.samePiece(board.padPiece)) {
      val symbols = currentPiece.convertPos
      val centerY = coords2._2 + (h2 / 2)
      val centerX = coords2._1 + (w2 / 5) + 5
      val img = if (currentPiece.position % 2 != 1) triangleDownImg else triangleUpImg

      g.drawImage(img, centerX, centerY, null)
      g.drawString(symbols._1.toString, centerX + (side / 4).toInt + (if (img == triangleDownImg) 15 else 20), centerY + (diameter / 2).toInt) //left
      g.drawString(symbols._2.toString, centerX + (side * 3 / 4).toInt - (if (img == triangleDownImg) 20 else 30), centerY + (diameter / 2).toInt) //right
      if (currentPiece.position % 2 != 1) {
        g.drawString(symbols._3.toString, centerX + (side / 2).toInt, centerY + 25) //top
    } else {
        g.drawString(symbols._4.toString, centerX + (side / 2).toInt, centerY + diameter.toInt - 15)  //bottom
     }
    }

    // Draws the pieces that are currently on the board
    for {
      i <- 0 until board.rows
      j <- 0 until board.columns
    } {
      val (x11, y11) = (boardCorner2._1 - (side / 2).toInt, boardCorner2._2)
      val (x21, y21) = (boardCorner1._1, boardCorner1._2 - diameter.toInt)
      val (added_x, added_y) = ((side / 2), diameter)
      val piece = board.getBoard(i)(j).getOrElse(board.padPiece)

      if (!piece.samePiece(board.padPiece)) {
        val symbols = piece.convertPos
        val img = if (piece.position % 2 == 1) triangleUpImg else triangleDownImg
        val x = if (i == 1 || i == 4) x11 + ((j - 2) * added_x).toInt else x21 + ((j - 1) * added_x).toInt
        val y = if (i == 1 || i == 4) y11 + ((i - 1) * added_y).toInt else y21 + ((i - 2) * added_y).toInt

      g.drawImage(img, x, y, null)
      g.drawString(symbols._1.toString, x + (side / 4).toInt + (if (piece.position % 2 == 1) 20 else 15), y + (diameter / 2).toInt)
      g.drawString(symbols._2.toString, x + (side * 3 / 4).toInt - (if (piece.position % 2 == 1) 30 else 20), y + (diameter / 2).toInt)
      if (piece.position % 2 == 1) {
        g.drawString(symbols._4.toString, x + (side / 2).toInt, y + diameter.toInt - 15)
      } else {
        g.drawString(symbols._3.toString, x + (side / 2).toInt, y + 25)
      }
     }
    }

    // Draws the piece that is being moved
     movedPiece.foreach { moved =>
     val symbols = moved.convertPos
     val img = if (moved.position % 2 != 1) triangleDownImg else triangleUpImg
      g.drawImage(img, movedCoords._1, movedCoords._2, null)
      g.drawString(symbols._1.toString, movedCoords._1 + (side / 4).toInt + (if (img == triangleDownImg) 15 else 20), movedCoords._2 + (diameter / 2).toInt)
      g.drawString(symbols._2.toString, movedCoords._1 + (side * 3 / 4).toInt - (if (img == triangleDownImg) 20 else 30), movedCoords._2 + (diameter / 2).toInt)
      if (img == triangleDownImg)
        g.drawString(symbols._3.toString, movedCoords._1 + (side / 2).toInt, movedCoords._2 + 25)
      else {
        if (img == triangleUpImg)
        g.drawString(symbols._4.toString, movedCoords._1 + (side / 2).toInt, movedCoords._2 + diameter.toInt - 15)
      }
    }
  }

  // Converts the coords on the screen given in pixels to match the indexes in the board array

  private def convertCoords(x: Int, y: Int): (Int, Int) = {
    val (x11, y11) = (boardCorner2._1 - (side / 2).toInt, boardCorner2._2)
    val (x11_side, y11_side) = (x11 + (side / 4).toInt, y11 + (diameter / 2).toInt)
    val (added_x, added_y) = ((side / 2), (diameter / 2))
    val xtwo = if (x >= x11_side && x <= (x11_side + added_x.toInt)) true
    val xfour = if (x >= (x11_side + (2 * added_x).toInt) && x <= (x11_side + (3 * added_x).toInt)) true
    val xsix = if (x >= (x11_side + (4 * added_x).toInt) && x <= (x11_side + (5 * added_x).toInt)) true
    val xthree = if (x >= (x11_side + added_x.toInt) && x <= (x11_side + (2 * added_x).toInt)) true
    val xfive = if (x >= (x11_side + (3 * added_x).toInt) && x <= (x11_side + (4 * added_x).toInt)) true
    val xseven = if (x >= (x11_side + (5 * added_x).toInt) && x <= (x11_side + (6 * added_x).toInt)) true
    val xone = if (x >= (x11_side - added_x.toInt) && x <= x11_side) true

    if (y >= y11_side && y <= (y11_side + added_y.toInt))
        if (xtwo == true) (1, 2) else if (xfour == true) (1, 4) else if (xsix == true) (1, 6) else
              (-1, -1)

    else if (y >= y11 && y <= y11_side)
        if (xthree == true) (1, 3) else if (xfive == true) (1, 5) else
            (-1, -1)

    else if (y >= (y11_side + (2 * added_y).toInt) && y <= (y11_side + (3 * added_y).toInt))
        if (xone == true) (2, 1) else if (xthree == true) (2, 3) else if (xfive == true) (2, 5) else if (xseven == true) (2, 7) else
                (-1, -1)

    else if (y >= (y11_side + added_y.toInt) && y <= (y11_side + (2 * added_y).toInt))
        if (xtwo == true) (2, 2) else if (xfour == true) (2, 4) else if (xsix == true) (2, 6) else
              (-1, -1)

    else if (y >= (y11_side + (3 * added_y).toInt) && y <= (y11_side + (4 * added_y).toInt))
        if (xone == true) (3, 1) else if (xthree == true) (3, 3) else if (xfive == true) (3, 5) else if (xseven == true) (3, 7) else
                (-1, -1)

    else if (y >= (y11_side + (4 * added_y).toInt) && y <= (y11_side + (5 * added_y).toInt))
        if (xtwo == true) (3, 2) else if (xfour == true) (3, 4) else if (xsix == true) (3, 6) else
              (-1, -1)

    else if (y >= (y11_side + (5 * added_y).toInt) && y <= (y11_side + (6 * added_y).toInt))
        if (xtwo == true) (4, 2) else if (xfour == true) (4, 4) else if (xsix == true) (4, 6) else
              (-1, -1)

    else if (y >= (y11_side + (6 * added_y).toInt) && y <= (y11_side + (7 * added_y).toInt))
        if (xthree == true) (4, 3) else if (xfive == true) (4, 5) else
            (-1, -1)

    else (-1, -1)
  }

  // Called when mouse is pressed, if this occurs on the stack or on a piece on the board, the piece is rotated
  private def onPress(x: Int, y: Int) = {
    val (stackX, stackY) = (coords2._1 + (w2 / 5) + 5, coords2._2 + (h2 / 2))

    // Rotate the piece in the stack if the click occurs within the bounds of the stack
    if ((x >= stackX && x <= (stackX + side)) && (y >= stackY && y <= (stackY + diameter))) {
      val current = stack.currentPiece.getOrElse(board.padPiece)
      if (!current.samePiece(board.padPiece)) {
        current.rotate()
      }
    }

    // Rotate the symbols on the piece, if the click occurs within the bounds of a piece on the board
    val converted = convertCoords(x, y)
    if (converted._1 != -1) {
      val current = board.getBoard(converted._1)(converted._2).getOrElse(board.padPiece)
      if (!current.samePiece(board.padPiece)) {
        current.rotate()
        current.rotate()
      }
    }
  }

  // Called when a mouse is moved, also updates the value of movedPiece, the piece that is currently moved
  private def onMove(x: Int, y: Int) = {
    val (stackX, stackY) = (coords2._1 + (w2 / 5) + 5, coords2._2 + (h2 / 2))

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
  private def onRelease(x: Int, y: Int): Unit = {
    val (stackX, stackY) = (coords2._1 + (w2 / 5) + 5, coords2._2 + (h2 / 2))
    val current = movedPiece.getOrElse(board.padPiece)
    val converted = convertCoords(x, y)

    // If the end coordinates are wrong, the piece is added back to the stack
    if ((startCoords._1 >= stackX && startCoords._1 <= (stackX + side)) && (startCoords._2 >= stackY && startCoords._2 <= (stackY + diameter))) {
      if (converted._1 != -1 && !current.samePiece(board.padPiece) && movedPiece.isDefined){
          if (converted._1 == 1 || converted._1 == 4) {
            val success = board.addPiece(current, converted._2 - 1, converted._1)
            if (!success) { stack.addPiece(current) }
          }
          if (converted._1 == 2 || converted._1 == 3) {
            val success = board.addPiece(current, converted._2, converted._1)
            if (!success) stack.addPiece(current)
          }
      } else {
        if (!current.samePiece(board.padPiece)) { stack.addPiece(current) }
      }

    // The start coords are on top of the stack. If the end coords are wrong, the piece is added back to the stack.
    } else {
      val convertedStart = convertCoords(startCoords._1, startCoords._2)
      if (convertedStart._1 != -1 && converted._1 != -1 && !current.samePiece(board.padPiece) && movedPiece.isDefined) {
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


  // Main Panel, contains all of the buttons and listens to them and the mouse
  private class MainPanel extends BoxPanel(Orientation.Vertical) {

    // Draws the image
    override def paintComponent(g: Graphics2D) = {
      onPaint(g)
    }

    private val actionDim = new Dimension(w2 / 3, h2 / 16)
    private val directionDim = new Dimension(w2 / 5, h2 / 16)
    private val buttonFont = new Font("Arial", java.awt.Font.PLAIN, w2 / 28)

    // The buttons
    private val newGame = new Button {
      text = "New game"
      font = buttonFont
      minimumSize = actionDim
      preferredSize = actionDim
      maximumSize = actionDim
    }
    private val continue = new Button {
      font = buttonFont
      text = "Continue game"
      minimumSize = actionDim
      preferredSize = actionDim
      maximumSize = actionDim
    }
    private val save = new Button {
      font = buttonFont
      text = "Save"
      minimumSize = actionDim
      preferredSize = actionDim
      maximumSize = actionDim
    }
    private val next = new Button {
      text = "->"
      minimumSize = directionDim
      preferredSize = directionDim
      maximumSize = directionDim
    }
    private val previous = new Button {
      text = "<-"
      minimumSize = directionDim
      preferredSize = directionDim
      maximumSize = directionDim
    }

    // Combining panels into one structure
    private val buttons11 = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(3 * width / 32 + 3 * width / 100)
      contents += newGame
      contents += Swing.HStrut(2 * width / 100)
      contents += continue
      background = new Color(0, 0, 0, 0)
    }
    private val buttons12 = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(3 * width / 32 + 3 * width / 100)
      contents += Swing.HStrut(2 * width / 100)
      contents += save
      background = new Color(0, 0, 0, 0)
    }
    private val buttons1 = new BoxPanel(Orientation.Vertical) {
      contents += buttons11
      contents += Swing.VStrut(-1 * height / 3 + 3 * height / 100)
      contents += buttons12
      background = new Color(0, 0, 0, 0)
    }
    private val buttons2 = new BoxPanel(Orientation.Horizontal) {
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
    private def startMessage(): Unit = {
      val text = "Welcome to the Triangle Puzzle! You started a new game. These are the rules: \n" +
      "You must solve the puzzle by dragging and matching the sides of pieces: uppercase to lowercase and vice versa. \n" +
      "You can click on the arrows to swipe through the stack of pieces and click on the piece on top to rotate it.\n" +
      "You can save your game and continue it later by pressing on the 'Save' button, which will close the game. \n" +
      "When you want to continue the game open the app again and click on 'Continue game'\n"

      Dialog.showMessage(contents(1), text, title= "Game started")
    }
    private def continueMessage(): Unit = {
      val text = "The last saved game was continued"
      Dialog.showMessage(contents(1), text, title= "Game continued")
    }
    private def saveMessage(): Unit = {
      val text = "The game was successfully saved"
      val res = Dialog.showConfirmation(contents(1), text, title= "Game saved")
    }
    private def endMessage(): Unit = {
      val text = "Congratulations! You solved the puzzle!"
      Dialog.showMessage(contents(1), text, title="Puzzle solved")
      game.endGame()
      repaint()
    }

    // Listens to the buttons new game and continue at the beginning
    listenTo(newGame)
    listenTo(continue)

    // Starts to listen to actions
    private def addGameListeners(): Unit = {
      listenTo(save, next, previous, mouse.clicks, mouse.moves)
      deafTo(newGame, continue)
}

  //Stops certain functions when activated
    private def addEndGameListeners(): Unit = {
      listenTo(newGame, continue)
      deafTo(save, next, previous, mouse.clicks, mouse.moves)
}
  // When buttons are clicked
  reactions += {
  case ButtonClicked(`newGame`) =>
    game.startGame()
    addGameListeners()
    deafTo(newGame, continue)
    repaint()
    startMessage()

  case ButtonClicked(`continue`) =>
    val gameSituation = FileOps.readFromFile("gameFile.txt")
    game.continueGame(gameSituation._1, gameSituation._2, gameSituation._3)
    addGameListeners()
    deafTo(newGame, continue)
    repaint()
    continueMessage()

  case ButtonClicked(`save`) =>
    FileOps.writeToFile("gameFile.txt", board.getBoard, stack.pieceStack, game.solution)
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
      addEndGameListeners()
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
      addEndGameListeners()
      endMessage()
    }
   }
  }
 }
