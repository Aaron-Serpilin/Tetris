package tetris.logic

import engine.graphics.Rectangle
import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

case class TetrisBlock(color: CellType, shape: List[Point], rotation: Int)

class TetrisLogic(val randomGen: RandomGenerator, val gridDims: Dimensions, val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims: Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  private val gameIsOver = false
  private val blockTypes: List[CellType] = List(ICell, JCell, LCell, OCell, SCell, TCell, ZCell)
  private var currentBlock: TetrisBlock = _
  private var tetrisBlocks: List[TetrisBlock] = List()

  def createBlock(): Unit = {
    val numberPossibleRotations = 4
    val randomRotationIndex = randomGen.randomInt(numberPossibleRotations)
    var anchorPoint = 0

    if (gridDims.width % 2 == 0) {
      anchorPoint = gridDims.width / 2
    } else {
      anchorPoint = (gridDims.width / 2) + 1
    }

    val randomShapeIndex = randomGen.randomInt(blockTypes.length)
    val randomShape = blockTypes(randomShapeIndex)

    randomShape match {
      case ICell => currentBlock = TetrisBlock(ICell, List(Point(anchorPoint - 2, 1), Point(anchorPoint - 1, 1), Point(anchorPoint, 1), Point(anchorPoint + 1, 1)), randomRotationIndex)
      case JCell => currentBlock = TetrisBlock(JCell, List(Point(anchorPoint - 2, 0), Point(anchorPoint - 2, 1), Point(anchorPoint - 1, 1), Point(anchorPoint, 1)), randomRotationIndex)
      case LCell => currentBlock = TetrisBlock(LCell, List(Point(anchorPoint - 2, 1), Point(anchorPoint - 1, 1), Point(anchorPoint, 1), Point(anchorPoint, 0)), randomRotationIndex)
      case OCell => currentBlock = TetrisBlock(OCell, List(Point(anchorPoint - 1, 0), Point(anchorPoint, 0), Point(anchorPoint - 1, 1), Point(anchorPoint, 1)), randomRotationIndex)
      case SCell => currentBlock = TetrisBlock(SCell, List(Point(anchorPoint - 2, 1), Point(anchorPoint - 1, 1), Point(anchorPoint - 1, 0), Point(anchorPoint, 0)), randomRotationIndex)
      case TCell => currentBlock = TetrisBlock(TCell, List(Point(anchorPoint - 2, 1), Point(anchorPoint - 1, 1), Point(anchorPoint, 1), Point(anchorPoint - 1, 0)), randomRotationIndex)
      case ZCell => currentBlock = TetrisBlock(ZCell, List(Point(anchorPoint - 1, 1), Point(anchorPoint, 1), Point(anchorPoint - 1, 0), Point(anchorPoint - 2, 0)), randomRotationIndex)
    }
  }


  def rotateLeft(): Unit = {
    //(x,y) -> (-y + 1,x)
  }

  def rotateRight(): Unit = {
    //(x,y) -> (y ,-x + 1)
  }

  def moveLeft(): Unit = {
    val lowestBlockWidth = currentBlock.shape.map(_.x).min // Returns the x-coordinate of the block with the lowest value
    if (lowestBlockWidth > 0) {
      currentBlock = currentBlock.copy(shape = currentBlock.shape.map(point => point.copy(x = point.x - 1)))
    }
  }

  def moveRight(): Unit = {
    val highestBlockWidth = currentBlock.shape.map(_.x).max // Returns the x-coordinate of the block with the highest value
    if (highestBlockWidth + 1 < gridDims.width) {
      currentBlock = currentBlock.copy(shape = currentBlock.shape.map(point => point.copy(x = point.x + 1)))
    }
  }

  private def canMoveDown(): Boolean = {
    val highestBlockHeight = currentBlock.shape.map(_.y).max // Returns the y-coordinate of the block with the highest value
    val potentialNewPositions = currentBlock.shape.map(point => point.copy(y = point.y + 1)) // Check if moving down would collide with existing blocks
    val collidesWithExistingBlocks = potentialNewPositions.exists(newPosition =>
      tetrisBlocks.exists(existingBlock =>
        existingBlock.shape.contains(newPosition)
      )
    )

    highestBlockHeight + 1 < gridDims.height && !collidesWithExistingBlocks
  }


  def moveDown(): Unit = {

    if (canMoveDown()) {
      currentBlock = currentBlock.copy(shape = currentBlock.shape.map(point => point.copy(y = point.y + 1)))
    } else {
      tetrisBlocks = currentBlock +: tetrisBlocks // We store all the blocks in a List once they've reached their final positions
      createBlock()
    }

  }

  def doHardDrop(): Unit = {
    while (canMoveDown()) {
      moveDown()
    }
  }


  def isGameOver: Boolean = gameIsOver

  def getAllBlocks: List[TetrisBlock] = currentBlock +: tetrisBlocks

  def getCellType(p: Point): CellType = {

    if (currentBlock == null) {
      createBlock()
    }

    if (getAllBlocks.exists(_.shape.contains(p))) { // We check for the currentBlock and all previously placed blocks
      getAllBlocks.find(_.shape.contains(p)).get.color // If it exists, we find it and return its color
    } else {
      Empty
    }
  }

}

object TetrisLogic {

  val FramesPerSecond: Int = 1
  val DrawSizeFactor = 1.0

  def makeEmptyBoard(gridDims: Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def apply(): TetrisLogic = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))
}
