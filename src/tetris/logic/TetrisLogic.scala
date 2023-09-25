package tetris.logic

import engine.graphics.Rectangle
import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

//case class GameState ()

case class TetrisBlock(shape: CellType, blockPositions: List[Point], rotation: Int)

class TetrisLogic(val randomGen: RandomGenerator, val gridDims : Dimensions, val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  private val blockTypes: Array[CellType] = Array(ICell, JCell, LCell, OCell, SCell, TCell, ZCell)
  private var currentBlock: TetrisBlock = _

  def spawnBlock(): Unit = {
    val numberPossibleRotations = 4
    val randomShapeIndex = randomGen.randomInt(blockTypes.length)
    val randomRotationIndex = randomGen.randomInt(numberPossibleRotations)
    val randomShape = blockTypes(randomShapeIndex)
    currentBlock = TetrisBlock(randomShape, List(Point(3, 0), Point(4, 0), Point(5, 0), Point(6, 0)), rotation = randomRotationIndex)
  }

  def rotateLeft(): Unit = ()

  def rotateRight(): Unit = ()

  def moveLeft(): Unit = ()

  def moveRight(): Unit = ()

  def moveDown(): Unit = ()

  def doHardDrop(): Unit = ()

  def isGameOver: Boolean = false

  def getCellType(p : Point): CellType = {
    spawnBlock()
    if (currentBlock.blockPositions.contains(p)) {
      ICell
    } else {
      Empty
    }
  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 10
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