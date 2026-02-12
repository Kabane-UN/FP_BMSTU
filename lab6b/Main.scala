class Matrix[T](val data: Vector[Vector[T]])(implicit numeric: Numeric[T] = null) {
  require(data.nonEmpty && data.forall(_.size == data.size), 
  "Matrix must be square and non-empty")
  
  val size: Int = data.size
  
  def removeRowAndColumn(i: Int, j: Int): Matrix[T] = {
    require(i >= 0 && i < size && j >= 0 && j < size, "Index out of bounds")
    
    val newData = data
      .zipWithIndex
      .filter { case (_, rowIdx) => rowIdx != i }
      .map { case (row, _) => 
        row.zipWithIndex.filter { case (_, colIdx) => colIdx != j }.map(_._1)
      }
    
    new Matrix[T](newData)
  }
  
  def determinant(implicit ops: Numeric[T] = numeric): Option[T] = {
    if (ops == null) None
    else Some(calculateDet(data))
  }
  private def calculateDet(matrix: Vector[Vector[T]])(implicit ops: Numeric[T]): T = {
    if (matrix.size == 1) {
      matrix.head.head
    } else if (matrix.size == 2) {
      val a = matrix(0)(0)
      val b = matrix(0)(1)
      val c = matrix(1)(0)
      val d = matrix(1)(1)
      ops.minus(ops.times(a, d), ops.times(b, c))
    } else {
      calculateDetTailRec(matrix, 0, ops.zero, 1)
    }
  }
  
  @annotation.tailrec
  private def calculateDetTailRec(
    matrix: Vector[Vector[T]],
    colIndex: Int,
    accumulator: T,
    sign: Int
  )(implicit ops: Numeric[T]): T = {
    if (colIndex >= matrix.size) {
      accumulator
    } else {
      val element = matrix.head(colIndex)
      val minor = removeRowAndColumnFromData(matrix, 0, colIndex)
      val minorDet = calculateDet(minor)
      val term = ops.times(element, minorDet)
      
      val newAccumulator = if (sign > 0) {
        ops.plus(accumulator, term)
      } else {
        ops.minus(accumulator, term)
      }
      calculateDetTailRec(matrix, colIndex + 1, newAccumulator, -sign)
    }
  }
  
  private def removeRowAndColumnFromData(
    matrix: Vector[Vector[T]], 
    row: Int, 
    col: Int
  ): Vector[Vector[T]] = {
    matrix
      .zipWithIndex
      .filter { case (_, i) => i != row }
      .map { case (r, _) => 
        r.zipWithIndex.filter { case (_, j) => j != col }.map(_._1)
      }
  }
  
  override def toString: String = {
    data.map(_.mkString("[", ", ", "]")).mkString("\n")
  }
}

object Matrix {
  def apply[T](data: Vector[Vector[T]])
  (implicit numeric: Numeric[T] = null): Matrix[T] = 
    new Matrix[T](data)
  
  def fill[T](n: Int, value: T)(implicit numeric: Numeric[T] = null): Matrix[T] = 
    new Matrix[T](Vector.fill(n, n)(value))
}

object Main extends App {
  import Numeric.Implicits._
  
  println("=== Integer Matrix ===")
  val intMatrix = Matrix(Vector(
    Vector(1, 2, 3),
    Vector(4, 5, 6), 
    Vector(7, 8, 9)
  ))
  
  println("Original matrix:")
  println(intMatrix)
  
  println("\nAfter removing row 1, column 1:")
  val reducedInt = intMatrix.removeRowAndColumn(1, 1)
  println(reducedInt)
  
  println("\nDeterminant of original matrix:")
  println(intMatrix.determinant)
  
  println("\nDeterminant of reduced matrix:")
  println(reducedInt.determinant)
  
  println("\n=== Double Matrix ===")
  val doubleMatrix = Matrix(Vector(
    Vector(1.5, 2.5),
    Vector(3.5, 4.5)
  ))
  
  println("Double matrix:")
  println(doubleMatrix)
  println(s"Determinant: ${doubleMatrix.determinant}")
  
  println("\n=== String Matrix ===")
  val stringMatrix = Matrix(Vector(
    Vector("a", "b"),
    Vector("c", "d")
  ))
  
  println("String matrix:")
  println(stringMatrix)
  
  println("\nAfter removing row 0, column 1:")
  val reducedString = stringMatrix.removeRowAndColumn(0, 1)
  println(reducedString)
  
  println("\nDeterminant for string matrix:")
  println(stringMatrix.determinant)
  
  println("\n=== 4x4 Integer Matrix ===")
  val matrix4x4 = Matrix(Vector(
    Vector(2, 1, 3, 4),
    Vector(1, 0, 2, 1), 
    Vector(3, 2, 1, 0),
    Vector(1, 1, 2, 2)
  ))
  
  println("4x4 matrix:")
  println(matrix4x4)
  println(s"Determinant: ${matrix4x4.determinant}")
}