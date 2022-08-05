/* eslint-disable no-console */
import * as R from 'ramda'

enum Piece {
  QUEEN = '♛',
  BISHOP = '♝',
  ROOK = '♜',
}

enum TableSquare {
  OCCUPIED = '⬛',
  EMPTY = '⬜',
  WALKABLE = '🟦',
}

type Square = Piece | TableSquare

type ChessTable = Square[][]

type Position = [number, number]

enum Direction {
  N = 'N',
  NE = 'NE',
  E = 'E',
  SE = 'SE',
  S = 'S',
  SW = 'SW',
  W = 'W',
  NW = 'NW',
}

const getNextPos = ([x, y]: Position, direction: Direction): Position => {
  const directionMap: Record<Direction, Position> = {
    [Direction.N]: [x, y + 1],
    [Direction.NE]: [x + 1, y + 1],
    [Direction.E]: [x + 1, y],
    [Direction.SE]: [x + 1, y - 1],
    [Direction.S]: [x, y - 1],
    [Direction.SW]: [x - 1, y - 1],
    [Direction.W]: [x - 1, y],
    [Direction.NW]: [x - 1, y + 1],
  }
  return directionMap[direction]
}

const getSquare = (table: ChessTable, [x, y]: Position): Square | null => {
  try {
    return table[x][y]
  } catch (_e) {
    return null
  }
}

const isFree = (table: ChessTable, pos: Position): boolean =>
  getSquare(table, pos) === TableSquare.EMPTY

const emptyTable = (): ChessTable =>
  Array(8).fill(Array(8).fill(TableSquare.EMPTY))

const rndNumber: () => number = R.pipe(Math.random, R.multiply(8), Math.floor)

const rndPos = (): Position => [rndNumber(), rndNumber()]

const rndTable = (occupied: number): ChessTable =>
  R.reduce(
    (table, _) => updateTable(table, rndPos(), TableSquare.OCCUPIED),
    emptyTable(),
    Array(occupied).fill(1),
  )

const printTable: (t: ChessTable) => void = R.pipe(R.transpose, console.table)

const updateTable = (
  table: ChessTable,
  pos: Position,
  piece: Square,
): ChessTable => R.set(R.lensPath<ChessTable>(pos), piece, table)

const walkInDirection = (
  table: ChessTable,
  pos: Position,
  direction: Direction,
): ChessTable => {
  const nextPos = getNextPos(pos, direction)
  if (isFree(table, nextPos)) {
    const newTable = updateTable(table, nextPos, TableSquare.WALKABLE)
    return walkInDirection(newTable, nextPos, direction)
  }
  return table
}

const walkInSign =
  (directions: Direction[]) =>
  (table: ChessTable, pos: Position): ChessTable =>
    R.reduce(
      (table, dir) => walkInDirection(table, pos, dir),
      table,
      directions,
    )

const walkInPlus = walkInSign([
  Direction.N,
  Direction.S,
  Direction.E,
  Direction.W,
])

const walkInX = walkInSign([
  Direction.NE,
  Direction.SE,
  Direction.SW,
  Direction.NW,
])

const walkInStar = walkInSign(R.values(Direction))

const walk = (table: ChessTable, pos: Position, piece: Piece): ChessTable => {
  const tableWithPiece = updateTable(table, pos, piece)
  if (piece === Piece.BISHOP) {
    return walkInX(tableWithPiece, pos)
  }
  if (piece === Piece.ROOK) {
    return walkInPlus(tableWithPiece, pos)
  }
  return walkInStar(tableWithPiece, pos)
}

const getWalkable = (table: ChessTable): Position[] => {
  const tablePositions: Position[] = R.xprod(R.range(0, 8), R.range(0, 8))
  return R.reduce(
    (positions, pos) =>
      getSquare(table, pos) === TableSquare.WALKABLE
        ? [...positions, pos]
        : positions,
    [] as Position[],
    tablePositions,
  )
}

R.forEach(piece => {
  const res = walk(rndTable(10), [2, 2], piece)
  printTable(res)
  console.log(getWalkable(res))
}, R.values(Piece))
