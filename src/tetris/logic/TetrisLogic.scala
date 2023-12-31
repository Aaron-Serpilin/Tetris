package tetris.logic

//import engine.graphics.Rectangle
import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

case class GameState (
                     gameIsOver: Boolean,
                     currentBlock: Tetromino,
                     absoluteBlockShape: List[Point],
                     relativeBlockShape: List[Point],
                     currentBlockType: CellType,
                     anchorPosition: Point,
                     tetrisBlocks: List[(CellType, List[Point])]
                     )

class TetrisLogic(val randomGen: RandomGenerator, val gridDims: Dimensions, val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims: Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  private var currentGameState: GameState = GameState(
    gameIsOver = false,
    currentBlock = null,
    absoluteBlockShape = List(),
    relativeBlockShape = List(),
    currentBlockType = Empty,
    anchorPosition = Point(0, 0),
    tetrisBlocks = List()
  )

  private val blockTypes: List[CellType] = List(ICell, JCell, LCell, OCell, SCell, TCell, ZCell)

 currentGameState = currentGameState.copy(anchorPosition =  if (gridDims.width % 2 == 0) Point((gridDims.width / 2) - 1, 1) else Point(gridDims.width / 2, 1))

  def relativeToAbsolutePositions (blockShape: List[Point]): List[Point] = {
    val adaptedShape = blockShape.map{point =>
      val absoluteX = point.x + currentGameState.anchorPosition.x
      val absoluteY = point.y + currentGameState.anchorPosition.y
      Point(absoluteX, absoluteY)
    }
    adaptedShape
  }

  def createTetromino (tetrominoShape: CellType): Tetromino = {
    val tetromino = tetrominoShape match {
      case ICell => new ICellBlock(currentGameState)
      case OCell => new OCellBlock(currentGameState)
      case JCell | LCell | SCell | TCell | ZCell => new standardBlock(currentGameState, tetrominoShape)
    }
    tetromino
  }

  def createBlock(): Unit = {
    val randomShapeIndex = randomGen.randomInt(blockTypes.length)
    val randomShape = blockTypes(randomShapeIndex)
    val tetromino = createTetromino(randomShape)

    val relativePositions = tetromino.initialPositions() // Use the relativePositions to account for change around the anchor
    val absolutePositions = relativeToAbsolutePositions(relativePositions) // Transform to absolutePositions for functionality

    val isOccupied = absolutePositions.exists(position => // Check if any of the spawning positions are occupied
      currentGameState.tetrisBlocks.exists {
        case (_, existingBlockPositions) => existingBlockPositions.contains(position)
      }
    )

    if (isOccupied) {
      currentGameState = currentGameState.copy(gameIsOver = true) // If any spawning position is occupied, set gameIsOver to true
    } else {
      currentGameState = currentGameState.copy(
        currentBlock = tetromino,
        relativeBlockShape = relativePositions,
        absoluteBlockShape = absolutePositions,
        currentBlockType = randomShape
      )

      removeLines()
    }
  }

  private def isValidPosition(blockShape: List[Point]): Boolean = {
    blockShape.forall(point =>
      point.x >= 0 && point.x < gridDims.width && point.y >= 0 && point.y < gridDims.height && // Check for gridDims
        !currentGameState.tetrisBlocks.exists(existingBlock => //Check for existing blocks
          existingBlock._2.contains(point)
        )
    )
  }

  private def rotate(direction: String): Unit = {
    val currentTetromino = createTetromino(currentGameState.currentBlockType)
    val relativeRotatedTetromino = if (direction == "Left") currentTetromino.rotateLeft() else currentTetromino.rotateRight()
    val rotatedTetromino = relativeToAbsolutePositions(relativeRotatedTetromino)

    if (isValidPosition(rotatedTetromino)) {
      currentGameState = currentGameState.copy(
        relativeBlockShape = relativeRotatedTetromino,
        absoluteBlockShape = rotatedTetromino
      )
    }
  }

  def rotateLeft(): Unit = rotate("Left")
  def rotateRight(): Unit = rotate("Right")

  private def move(direction: String): Unit = {
    val (deltaX, deltaY) = direction match {
      case "Left" => (-1, 0)
      case "Right" => (1, 0)
      case "Down" => (0, 1)
    }

    val newAbsoluteBlockShape = currentGameState.absoluteBlockShape.map(point =>
      Point(point.x + deltaX, point.y + deltaY)
    )

    val newAnchorPosition = Point(
      currentGameState.anchorPosition.x + deltaX,
      currentGameState.anchorPosition.y + deltaY
    )

    if (isValidPosition(newAbsoluteBlockShape)) {
      currentGameState = currentGameState.copy(
        absoluteBlockShape = newAbsoluteBlockShape,
        anchorPosition = newAnchorPosition
      )
    }
  }

  def moveLeft(): Unit = move("Left")

  def moveRight(): Unit = move("Right")

  def moveDown(): Unit = {
    if (canMoveDown()) {
      move("Down")
    } else {
      val newTetrisBlock = (currentGameState.currentBlockType, currentGameState.absoluteBlockShape)
      val newTetrisBlocks = newTetrisBlock :: currentGameState.tetrisBlocks
      currentGameState = currentGameState.copy(
        tetrisBlocks = newTetrisBlocks,
        anchorPosition = if (gridDims.width % 2 == 0) Point((gridDims.width / 2) - 1, 1) else Point(gridDims.width / 2, 1)
      )
      createBlock()
    }

  }

  private def canMoveDown(): Boolean = {
    val potentialNewPositions = currentGameState.absoluteBlockShape.map(point => point.copy(y = point.y + 1))
    isValidPosition(potentialNewPositions)
  }

  def doHardDrop(): Unit = {
    while (canMoveDown()) {moveDown()}
  }

  def isGameOver: Boolean = currentGameState.gameIsOver

  private def readInitialBoard(): Unit = {
    val tetrisBlocks = initialBoard.zipWithIndex.flatMap {
      case (row, rowIndex) =>
        row.zipWithIndex.collect {
          case (cellType, columnIndex) if cellType != Empty =>
            val absolutePosition = Point(columnIndex, rowIndex)
            (cellType, List(absolutePosition))
        }
    }.toList

    currentGameState = currentGameState.copy(tetrisBlocks = tetrisBlocks)
  }

  private def findFullRows(): List[Int] = {
    var fullRows: List[Int] = List.empty

    for (row <- 0 until gridDims.height) { // Loop through all the rows of the board
      val rowOccupied = currentGameState.tetrisBlocks.exists { // rowOccupied set to true if there exists any tetris block in the row with the given y-coordinate
        case (_, blockPositions) => blockPositions.exists(_.y == row)
      }

      val isFull = rowOccupied && (0 until gridDims.width).forall { column => // Returns true if all the units of rowOccupied are occupied
        currentGameState.tetrisBlocks.exists {
          case (_, blockPositions) => blockPositions.contains(Point(column, row))
        }
      }

      if (isFull) { // If all units of rowOccupied are full, we add the row index to fullRows
        fullRows = row :: fullRows
      }
    }

    fullRows.reverse // Returns the list in the right order
  }

  private def removeLines(): Unit = {
    val fullRowIndexes = findFullRows()

    if (fullRowIndexes.nonEmpty) {
      val newTetrisBlocks = currentGameState.tetrisBlocks.flatMap { //We use flatMap so we can remove only the blocks in the fullRow, and not the entire blocks
        case (blockColor, blockPositions) =>
          val nonCompleteRows = blockPositions.filterNot(point => fullRowIndexes.contains(point.y)) // We filter out the rows that are part of fullRowIndexes
          val shiftedRows = nonCompleteRows.map(point => // We shift the rows that are not full and that are below the removedLine by the number of removedLines
            if (point.y < fullRowIndexes.min) {Point(point.x, point.y + fullRowIndexes.size)} else point
          )
          if (shiftedRows.nonEmpty) Some((blockColor, shiftedRows))
          else None
      }
      currentGameState = currentGameState.copy(tetrisBlocks = newTetrisBlocks)
    }
  }

  def getCellType(p: Point): CellType = {

    if (currentGameState.currentBlock == null) {
      readInitialBoard()
      createBlock()
    }
    if (currentGameState.absoluteBlockShape.contains(p)) return currentGameState.currentBlockType

    for (previousBlocks <- currentGameState.tetrisBlocks) {
      val previousBlockColor = previousBlocks._1
      val previousBlockShape = previousBlocks._2
      if (previousBlockShape.contains(p)) return previousBlockColor
    }

    Empty
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
