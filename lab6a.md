% Лабораторная работа № 6а «Введение в объетно-ориентированное
  программирование на языке Scala»
% 20 ноября 2025 г.
% Андрей Кабанов, ИУ9-11М

# Цель работы
Целью данной работы является изучение базовых 
объектно-ориентированных возможностей языка Scala.

# Индивидуальный вариант
Вещественная матрица размера m × n, где 0 < m, n < 109, конструируемая на основе 
функции, вычисляющей значение элемента с указанными координатами.

Операции: сложение («+»), умножение на число («*»), умножение на матрицу («*»), 
транспонирование («!»), удаление i-той строки («delRow»), удаление i-го столбца 
(«delColumn»), вычисление значения элемента с указанными координатами («get»). 
Операции должны порождать исключения при невозможности их применения к операндам.

# Реализация

```scala
import scala.collection.MapView

final class SparseRealMatrix private (
  val m: Int,
  val n: Int,
  private val data: Map[(Int, Int), Double]
) {

  require(m > 0 && n > 0, s"Размеры должны быть положительными: $m x $n")
  require(m < 1_000_000_000 && n < 1_000_000_000, s"Размеры слишком большие: $m x $n")

  def get(i: Int, j: Int): Double = {
    if (i < 1 || i > m || j < 1 || j > n)
      throw new IndexOutOfBoundsException(s"Недопустимый индекс: ($i,$j)")
    data.getOrElse((i, j), 0.0)
  }

  def +(other: SparseRealMatrix): SparseRealMatrix = {
    if (this.m != other.m || this.n != other.n)
      throw new IllegalArgumentException("Размеры не совпадают")

    val merged = (this.data.keySet ++ other.data.keySet)
      .flatMap { p =>
        val v = this.data.getOrElse(p, 0.0) + other.data.getOrElse(p, 0.0)
        if (v == 0.0) None else Some(p -> v)
      }.toMap

    new SparseRealMatrix(m, n, merged)
  }

  def *(k: Double): SparseRealMatrix =
    if (k == 0.0) new SparseRealMatrix(m, n, Map.empty)
    else new SparseRealMatrix(m, n, data.map { case (p, v) => p -> (v * k) })

  def *(other: SparseRealMatrix): SparseRealMatrix = {
    if (this.n != other.m)
      throw new IllegalArgumentException("Нельзя умножить: размеры не согласованы")

    val otherByRow: MapView[Int, Map[Int, Double]] =
      other.data.groupBy { case ((row, _), _) => row }
        .mapValues(_.map { case ((_, col), v) => (col, v) })

    val result = scala.collection.mutable.Map[(Int, Int), Double]()

    for (((i, k), v1) <- this.data) {
      otherByRow.get(k).foreach { rowMap =>
        for ((j, v2) <- rowMap) {
          val newVal = result.getOrElse((i, j), 0.0) + v1 * v2
          if (newVal != 0.0) result((i, j)) = newVal
          else result.remove((i, j)) 
        }
      }
    }

    new SparseRealMatrix(this.m, other.n, result.toMap)
  }

  def unary_! : SparseRealMatrix =
    new SparseRealMatrix(n, m, data.map { case ((i, j), v) => (j, i) -> v })

  def delRow(iRow: Int): SparseRealMatrix = {
    if (iRow < 1 || iRow > m) throw new IndexOutOfBoundsException("Нет такой строки")

    val newData = data.collect {
      case ((i, j), v) if i < iRow => ((i, j), v)
      case ((i, j), v) if i > iRow => ((i - 1, j), v)
    }

    new SparseRealMatrix(m - 1, n, newData)
  }

  def delColumn(jCol: Int): SparseRealMatrix = {
    if (jCol < 1 || jCol > n) throw new IndexOutOfBoundsException("Нет такого столбца")

    val newData = data.collect {
      case ((i, j), v) if j < jCol => ((i, j), v)
      case ((i, j), v) if j > jCol => ((i, j - 1), v)
    }

    new SparseRealMatrix(m, n - 1, newData)
  }

  override def toString: String = {
    val sb = new StringBuilder
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        val v = data.getOrElse((i, j), 0.0)
        sb.append(f"$v%8.4f")
      }
      sb.append("\n")
    }
    sb.toString
  }
}

object SparseRealMatrix {
  def apply(m: Int, n: Int)(f: (Int, Int) => Double): SparseRealMatrix = {
    val buf = scala.collection.mutable.Map[(Int, Int), Double]()
    for (i <- 1 to m; j <- 1 to n) {
      val v = f(i, j)
      if (v != 0.0) buf((i, j)) = v
    }
    new SparseRealMatrix(m, n, buf.toMap)
  }

  def fromArray(arr: Array[Array[Double]]): SparseRealMatrix = {
    val m = arr.length
    val n = arr(0).length
    val map = (for {
      i <- arr.indices
      j <- arr(i).indices
      if arr(i)(j) != 0.0
    } yield (i + 1, j + 1) -> arr(i)(j)).toMap
    new SparseRealMatrix(m, n, map)
  }
  def identity(k: Int): SparseRealMatrix =
    new SparseRealMatrix(k, k, (1 to k).map(i => (i, i) -> 1.0).toMap)
}


object DemoSparse extends App {
  val A = SparseRealMatrix.fromArray(Array(
    Array(1.0, 0, 3),
    Array(0.0, 5, 0)
  ))

  val B = SparseRealMatrix.apply(3, 2) { (i, j) =>
    if (i == j) 1.0 else 0.0
  }

  println("A:")
  println(A)

  println("B:")
  println(B)

  println("A * B:")
  println(A * B)

  println("!(A * B):")
  println(!(A * B))

  println("A + A:")
  println(A + A)

  println("A без 2-го столбца:")
  println(A.delColumn(2))

  println("2.5 * A:")
  println(A * 2.5)
}

```

# Тестирование

```scala
Compiling project (Scala 3.7.4, JVM (11))
Warning: there was 1 deprecation warning; re-run with -deprecation for details
Compiled project (Scala 3.7.4, JVM (11))
A:
  1.0000  0.0000  3.0000
  0.0000  5.0000  0.0000

B:
  1.0000  0.0000
  0.0000  1.0000
  0.0000  0.0000

A * B:
  1.0000  0.0000
  0.0000  5.0000

!(A * B):
  1.0000  0.0000
  0.0000  5.0000

A + A:
  2.0000  0.0000  6.0000
  0.0000 10.0000  0.0000

A без 2-го столбца:
  1.0000  3.0000
  0.0000  0.0000

2.5 * A:
  2.5000  0.0000  7.5000
  0.0000 12.5000  0.0000
```

# Вывод
В ходе работы были освоены основные принципы объектно-ориентированного 
программирования в Scala, включая создание классов, использование конструкторов и 
методов. Также была реализована работа с разреженными матрицами через хеш-таблицы, 
что позволило эффективно работать с большими матрицами, содержащими 
преимущественно нулевые элементы.