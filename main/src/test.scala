
import java.io.File
import javax.imageio.ImageIO
import scala.swing._
import scala.swing.event._
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel}

object Starter extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Swing Test APp"
    val button = new Button {
      text = "Click if you can"
    }
    val label = new Label {
      text = ") Clicks"
    }
    val panel = new BoxPanel(Orientation.Vertical)
    panel.contents += button
    panel.contents += label
    panel.border = Swing.EmptyBorder(30, 30, 10, 30)

    contents = panel


    listenTo(button)
    var numClicks = 0
    reactions += {
      case ButtonClicked(b) =>
        numClicks += 1
        label.text = numClicks.toString
    }

  }


  val imago = new ImageIcon("puzzlebackground.jpg")

}