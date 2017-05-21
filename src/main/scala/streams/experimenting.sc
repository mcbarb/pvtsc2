import streams.StringParserTerrain

case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = p => {
  try {
    levelVector(p.row)(p.col) match {
      case 'S' => true
      case 'T' => true
      case 'o' => true
      case _ => false
    }
  }
  catch {
    case e: IndexOutOfBoundsException => false
  }
}

val t = Vector(Vector('S','T'),Vector('o','o'))

terrainFunction(t)(Pos(0,0))
terrainFunction(t)(Pos(0,1))
terrainFunction(t)(Pos(1,0))
terrainFunction(t)(Pos(1,1))
terrainFunction(t)(Pos(1,2))

(0,t(0).indexWhere(x => x == 'Y'))

def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
  def p = (x: Char) => x == c
  (for {
    (row, i) <- levelVector.zipWithIndex
    if row.indexWhere(p) >= 0
  } yield Pos(i, row.indexWhere(p))).head
}

findChar('T',t)

//object Test extends StringParserTerrain {
//  override val level =
//    """ooo-------
//      |oSoooo----
//      |ooooooooo-
//      |-ooooooooo
//      |-----ooToo
//      |------ooo-"""
//}
//
//Test.findChar('S')


