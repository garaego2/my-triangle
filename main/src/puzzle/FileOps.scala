package puzzle

import java.io._
import scala.collection.mutable.Buffer

object FileOps {

   /* Takes in a filename, an array of the situation on the board, a vector of the stack,
    and an array of the solution for the game. Writes the information from these collections to a file. If
   * a file with the file name does not exist, it creates a new file with that name.  */
def writeToFile(fileName: String, gameSituation: Array[Array[Option[Piece]]], stack: Vector[Piece], gameSolution: Array[Array[Option[Piece]]]) = {
    try {
      val fileOut = new FileWriter(fileName)
      val linesOut = new BufferedWriter(fileOut)

      try {
        // A helper method for writing the information of a piece.
        def writePieceInfo(p: Piece) = {
          s"${p.left}${p.bottom}${p.right}${p.position.toString}"
        }

        // Writes the information for the situation on the board.
        linesOut.write("#GAME")
        linesOut.newLine()
        linesOut.newLine()

        linesOut.write("#BOARD")
        linesOut.newLine()

        for (i <- gameSituation.indices) {
          var string = ""
          for (j <- gameSituation(i).indices) {
            val onePiece = gameSituation(i)(j).getOrElse(new Piece('O', 'O', 'O', 1))
            if (onePiece.left != 'O' && onePiece.left != 'x') {
              string += s"${writePieceInfo(onePiece)}${onePiece.getCoords._1}${onePiece.getCoords._2} "
            }
          }
          linesOut.write(string.trim)
          linesOut.newLine()
        }
        linesOut.newLine()

        // Writes all of the pieces in the stack vector on separate rows.
        linesOut.write("#STACK")
        linesOut.newLine()

        for (i <- stack.indices) {
          linesOut.write(writePieceInfo(stack(i)))
          linesOut.newLine()
        }

        linesOut.newLine()

        // Writes the information for the solution of the game.
        linesOut.write("#SOLUTION")
        linesOut.newLine()

        for (i <- gameSolution.indices) {
          var string = ""
          for (j <- gameSolution(i).indices) {
            val onePiece = gameSolution(i)(j).getOrElse(new Piece('O', 'O', 'O', 1))
            if (onePiece.left != 'O' && onePiece.left != 'x') {
              string += s"${writePieceInfo(onePiece)}${onePiece.getCoords._1}${onePiece.getCoords._2} "
            }
          }
          linesOut.write(string.trim)
          linesOut.newLine()
        }
      } finally {
        linesOut.close()
        fileOut.close()
      }
     }
      catch {
      case notFound: FileNotFoundException => println("File not found")
      case e: IOException => println("Writing finished with error")
     }
    }

   /* Takes in a name of a file and reads the information from that file. If the file does
   * not exist, returns an exception. Returns a tuple with an array for board, a buffer for stack and array for the solution   */
  def readFromFile(fileName: String): (Array[Array[Option[Piece]]], Buffer[Piece], Array[Array[Option[Piece]]]) = {
    try {
      val fileIn = new FileReader(fileName)
      val linesIn = new BufferedReader(fileIn)

      try {
         // Variables that keep track of the information read
        var currentLine = linesIn.readLine()
        var header = ""
        var subHeader = ""
        var arr = Array.fill[Option[Piece]](6, 9)(Some(new Piece('x', 'x', 'x', 1)))
        val stack = Buffer[Piece]()
        var sol = Array.fill[Option[Piece]](6, 9)(Some(new Piece('x', 'x', 'x', 1)))

        if (currentLine.toLowerCase startsWith "#game") {
          header = "#game"
        }

         // A helper method that gets a string as a parameter and determines which piece the string is
        def piece(pieceInfo: String) = {
          val left = pieceInfo.head
          val bottom = pieceInfo.drop(1).head
          val right = pieceInfo.drop(2).head
          val position = pieceInfo.drop(3).head

          new Piece(left, right, bottom, position - '0')
        }

         // A helper method that gets a string as a parameter and determines the coordinates of a piece
        def piecePlace(placeInfo: String) = {
          val r = placeInfo.head
          val c = placeInfo.drop(1).head
          val row = r - '0'
          var col = c - '0'
          if (r == 1 || r == 4) { col += 1 }
          (row, col)
        }

         // A helper method for reading the information of an array from a file
        def readBoardInfo(s: String, a: Array[Array[Option[Piece]]]) = {
          val info = s.split(" ")
          for (p <- 0 until info.length) {
            val pie = piece(info(p).take(4).trim)
            val pla = piecePlace(info(p).drop(4).trim)
            if (pla._1 == 1 || pla._1 == 4) {
              a(pla._1)(pla._2 + 1) = Some(pie)
            } else {
              if (pla._1 == 2 || pla._1 == 3) {
                a(pla._1)(pla._2) = Some(pie)
              }
            }
          }
        }

        while({currentLine = linesIn.readLine(); currentLine != null}) {
          if (!currentLine.equals("") && currentLine != null) {
            currentLine = currentLine.trim

          currentLine.toLowerCase() match {
          case "#game" => header = "#game"
          case "#board" => subHeader = "#board"
          case "#stack" => subHeader = "#stack"
          case "#solution" => subHeader = "#solution"
          case _ => // do nothing
        }
      }

          // Reads the information for the board, stack and the solution
          if (header == "#game") {
            if (!(currentLine.toLowerCase startsWith "#") && !currentLine.equals("")) {
            if (subHeader == "#board") {
              readBoardInfo(currentLine, arr)
          }
            if (subHeader == "#stack") {
              stack += piece(currentLine.trim)
          }
            if (subHeader == "#solution" ) {
              readBoardInfo(currentLine, sol)
          }
        }
          }
        }

       // Returns the board, stack and the solution
       (arr, stack, sol)

      } finally {
        linesIn.close()
        fileIn.close()
      }

    } catch {
      // Catches exceptions in reading from the file
      case notFound: FileNotFoundException => {
        println("File not found")
        (Array(Array(None)), Buffer(), Array(Array(None)))
      }
      case e: IOException => {
        println("Reading finished with error")
        (Array(Array(None)), Buffer(), Array(Array(None)))
      }
    }
  }
}
