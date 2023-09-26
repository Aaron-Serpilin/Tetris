package tetris.logic

import engine.graphics.Rectangle
import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

//case class GameState ()

case class TetrisBlock(color: CellType, shape: List[Point], rotation: Int)

class TetrisLogic(val randomGen: RandomGenerator, val gridDims : Dimensions, val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  private val gameIsOver = false;
  private val blockTypes: List[CellType] = List(ICell, JCell, LCell, OCell, SCell, TCell, ZCell)
  private var currentBlock: TetrisBlock = _
  private val randomShapeIndex = randomGen.randomInt(blockTypes.length)
  private val randomShape = blockTypes(randomShapeIndex)

  def createBlock(): Unit = {
    val numberPossibleRotations = 4
    val randomRotationIndex = randomGen.randomInt(numberPossibleRotations)

    randomShape match {
      case ICell => currentBlock = TetrisBlock(ICell, List(Point(3, 1), Point(4, 1), Point(5, 1), Point(6, 1)), randomRotationIndex)
      case JCell => currentBlock = TetrisBlock(JCell, List(Point(3, 0), Point(3, 1), Point(4, 1), Point(5, 1)), randomRotationIndex)
      case LCell => currentBlock = TetrisBlock(LCell, List(Point(3, 1), Point(4, 1), Point(5, 1), Point(5, 0)), randomRotationIndex)
      case OCell => currentBlock = TetrisBlock(OCell, List(Point(4, 0), Point(4, 1), Point(5, 0), Point(5, 1)), randomRotationIndex)
      case SCell => currentBlock = TetrisBlock(SCell, List(Point(3, 1), Point(4, 1), Point(4, 0), Point(5, 0)), randomRotationIndex)
      case TCell => currentBlock = TetrisBlock(TCell, List(Point(3, 1), Point(4, 1), Point(4, 0), Point(5, 1)), randomRotationIndex)
      case ZCell => currentBlock = TetrisBlock(ZCell, List(Point(3, 0), Point(4, 0), Point(4, 1), Point(5, 1)), randomRotationIndex)
    }
  }

  def rotateLeft(): Unit = ()

  def rotateRight(): Unit = ()

  def moveLeft(): Unit = ()

  def moveRight(): Unit = ()

  def moveDown(): Unit = {
//    for (blockPoint <- currentBlock.shape) {
//      blockPoint.y -= 1
//    }
  }

  def doHardDrop(): Unit = ()

  def isGameOver: Boolean = gameIsOver

  def getCellType(p : Point): CellType = {
    createBlock()
    if (currentBlock.shape.contains(p)) {
      currentBlock.color
    } else {
      Empty
    }
  }

}

object TetrisLogic {

  val FramesPerSecond: Int = 1/100
  val DrawSizeFactor = 1.0

  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}