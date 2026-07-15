{-# LANGUAGE BangPatterns #-}

import Control.Monad (guard)
import Data.Array.IArray (accumArray, elems, listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.Bits
import Data.Ix (inRange)
import Data.List (mapAccumR)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Word (Word64)

type CellMask = Word64

data Clue = Clue {clueCell :: Int, clueScore :: Integer}
data Altitude = Board | Tower deriving (Eq, Show)
data Schedule = Schedule Int [Clue] (UArray Int Char) Altitude
type Witness = ([Int], String, CellMask)

-- Region letters flatten the printed board row by row.
regionRows :: [String]
regionRows =
  [ "AAAAABBB"
  , "CCCDDEEB"
  , "CFCDDDEB"
  , "GFFHHIEE"
  , "GGFHHIIJ"
  , "GKFLIIMJ"
  , "GKLLLMMJ"
  , "KKKLMMJJ"
  ]

regions :: String
regions = concat regionRows

side, cellCount, maxMoves, startCell :: Int
side = length regionRows
cellCount = length regions
maxMoves = cellCount - 1
startCell = cellCount - side

boardMask :: CellMask
boardMask = foldl' setBit 0 [0 .. maxMoves]

cellAt :: Int -> Int -> Int
cellAt row column = (row - 1) * side + column - 1

-- Printed order is input; visit order is derived.
clues :: [Clue]
clues =
  [ Clue (cellAt 1 6) 37
  , Clue (cellAt 1 8) 1100
  , Clue (cellAt 3 4) 23
  , Clue (cellAt 3 6) 138
  , Clue (cellAt 4 1) 528
  , Clue (cellAt 5 2) 449
  , Clue (cellAt 5 5) 16
  , Clue (cellAt 6 2) 750
  , Clue (cellAt 6 4) 88
  , Clue (cellAt 6 6) 272
  , Clue (cellAt 6 7) 1
  ]

-- Scores find candidate clues without fixing their visit order.
cluesByScore :: Map.Map Integer [(Int, Clue)]
cluesByScore = Map.fromListWith (++)
  [(clueScore clue, [(index, clue)]) | (index, clue) <- zip [0 ..] clues]

-- Record every 3 moves through 18, then every K moves.
earlyPeriod, earlyEnd :: Int
earlyPeriod = 3
earlyEnd = 18

clueCount, earlyClues :: Int
clueCount = length clues
earlyClues = earlyEnd `div` earlyPeriod

checkpointMove :: Int -> Int -> Int
checkpointMove index k
  | index < earlyClues = earlyPeriod * (index + 1)
  | otherwise = earlyEnd + k * (index - earlyClues + 1)

-- No revisits bound moves by the cell count.
feasibleKs :: [Int]
feasibleKs =
  takeWhile ((<= maxMoves) . checkpointMove (clueCount - 1))
    [earlyPeriod + 1 ..]

-- Same-level adds; up multiplies; exact down divides.
applyOperation :: Int -> Integer -> Altitude -> Char -> Maybe (Integer, Altitude)
applyOperation move score altitude 'S' = Just (score + fromIntegral move, altitude)
applyOperation move score Board _ = Just (score * fromIntegral move, Tower)
applyOperation move score Tower _
  | score `mod` amount == 0 = Just (score `div` amount, Board)
  | otherwise = Nothing
  where
    amount = fromIntegral move

-- Scores derive schedules before geometry enters the search.
deriveSchedules :: Int -> Altitude -> [Schedule]
deriveSchedules k startAltitude = searchArithmetic 0 1 0 0 [] [] startAltitude
  where
    searchArithmetic ::
      Int -> Int -> Integer -> Word64 -> [Clue] -> String -> Altitude -> [Schedule]
    searchArithmetic checkpoint move score usedClues clueOrder operations altitude
      | checkpoint == clueCount =
          [ Schedule k (reverse clueOrder)
              (listArray (0, move - 2) (reverse operations)) startAltitude
          ]
      | move > checkpointMove checkpoint k = do
          (index, clue) <- Map.findWithDefault [] score cluesByScore
          guard $ not (testBit usedClues index)
          searchArithmetic (checkpoint + 1) move score (setBit usedClues index)
            (clue : clueOrder) operations altitude
      | otherwise = do
          operation <- ['S', if altitude == Board then 'U' else 'D']
          (nextScore, nextAltitude) <-
            maybeToList (applyOperation move score altitude operation)
          searchArithmetic checkpoint (move + 1) nextScore
            usedClues clueOrder (operation : operations) nextAltitude

arithmeticSchedules :: [Schedule]
arithmeticSchedules = do
  k <- feasibleKs
  startAltitude <- [Board, Tower]
  deriveSchedules k startAltitude

-- A 3-D knight projects to distance² 5 level and 4 across heights.
moveTable :: Int -> UArray Int CellMask
moveTable squaredDistance = listArray (0, maxMoves) (map movesFrom [0 .. maxMoves])
  where
    offsets =
      [ (dr, dc)
      | dr <- [-2 .. 2]
      , dc <- [-2 .. 2]
      , dr * dr + dc * dc == squaredDistance
      ]
    movesFrom cell = foldl' setBit 0
      [ row * side + column
      | (dr, dc) <- offsets
      , let row = cell `div` side + dr
      , let column = cell `mod` side + dc
      , inRange ((0, 0), (side - 1, side - 1)) (row, column)
      ]

levelMoves, heightMoves, orthogonalNeighbors :: UArray Int CellMask
levelMoves = moveTable 5
heightMoves = moveTable 4
orthogonalNeighbors = moveTable 1

-- A region mask rejects a second tower in the same region.
regionMasks :: UArray Char CellMask
regionMasks = accumArray (.|.) 0 (minimum regions, maximum regions)
  [(region, bit cell) | (cell, region) <- zip [0 ..] regions]

cellRegionMask :: UArray Int CellMask
cellRegionMask = listArray (0, maxMoves) (map (regionMasks !) regions)

-- Visit set bits without allocating an index list.
eachBit :: CellMask -> (Int -> [a]) -> [a]
eachBit 0 _ = []
eachBit bits branch =
  branch (countTrailingZeros bits)
    ++ eachBit (bits .&. (bits - 1)) branch

-- Reserve future clues, then propagate reachability backward.
reachability :: Schedule -> UArray Int CellMask
reachability (Schedule k clueOrder fixedOperations _) =
  listArray (0, maxMoves) $
    reachablePrefix ++ replicate (maxMoves - lastClueMove) boardMask
  where
    lastClueMove = checkpointMove (clueCount - 1) k
    clueCellsByMove :: UArray Int CellMask
    clueCellsByMove = accumArray (.|.) 0 (0, lastClueMove)
      [ (checkpointMove index k, bit (clueCell clue))
      | (index, clue) <- zip [0 ..] clueOrder
      ]
    allowedCells = snd (mapAccumR reserveClue 0 (elems clueCellsByMove))
    reserveClue reserved 0 = (reserved, boardMask .&. complement reserved)
    reserveClue reserved required = (reserved .|. required, required)
    predecessors (operation, allowed) nextCells = allowed .&. unionMoves nextCells
      where
        moves = if operation == 'S' then levelMoves else heightMoves
        unionMoves 0 = 0
        unionMoves bits =
          moves ! countTrailingZeros bits
            .|. unionMoves (bits .&. (bits - 1))
    reachablePrefix =
      scanr predecessors (clueCellsByMove ! lastClueMove) $
        zip (elems fixedOperations) allowedCells

-- DFS adds cells, no-revisit, and one-tower-per-region constraints.
solveSchedule :: Schedule -> [Witness]
solveSchedule schedule@(Schedule k clueOrder fixedOperations startAltitude)
  | not (testBit (reachable ! 0) startCell) = []
  | otherwise = searchPaths 0 startCell (bit startCell) initialTowerCells
      startAltitude (clueScore (last clueOrder))
  where
    reachable = reachability schedule
    lastClueMove = checkpointMove (clueCount - 1) k
    finishLimit = min maxMoves (lastClueMove + k - 1)
    initialTowerCells =
      if startAltitude == Tower then cellRegionMask ! startCell else 0

    searchPaths !move !currentCell !visited !towerCells !altitude !tailScore
      | towerCells == boardMask && move >= lastClueMove = [([currentCell], [], visited)]
      | towerCells == boardMask = []
      | move == finishLimit = []
      | otherwise = do
          operation <-
            if move < lastClueMove
              then [fixedOperations ! move]
              else ['S', if altitude == Tower then 'D' else 'U']
          let nextAltitude =
                case operation of
                  'U' -> Tower
                  'D' -> Board
                  _ -> altitude
          nextTailScore <-
            if move < lastClueMove
              then [tailScore]
              else
                fst <$> maybeToList
                  (applyOperation (move + 1) tailScore altitude operation)
          let legalMoves = if operation == 'S' then levelMoves else heightMoves
              blocked = visited .|. (if nextAltitude == Tower then towerCells else 0)
              candidates =
                legalMoves ! currentCell
                  .&. complement blocked
                  .&. reachable ! (move + 1)
          eachBit candidates $ \nextCell ->
            let nextTowerCells =
                  if nextAltitude == Tower
                    then towerCells .|. cellRegionMask ! nextCell
                    else towerCells
            in
              [ (currentCell : path, operation : operations, finalVisited)
              | (path, operations, finalVisited) <-
                  searchPaths (move + 1) nextCell (setBit visited nextCell)
                    nextTowerCells nextAltitude nextTailScore
              ]

-- Replay scores and transpose the final neighbor sum onto path cells.
printWitness :: Schedule -> Witness -> IO ()
printWitness (Schedule k _ _ startAltitude) (path, operations, visited) =
  putStr $ unlines
    [ "K: " ++ show k
    , "start: " ++ show startAltitude
    , "operations: " ++ operations
    , "path: " ++ unwords (map showCell path)
    , "answer: " ++ show answer
    ]
  where
    scores = scanl applyScore (0 :: Integer) (zip [1 ..] operations)
    applyScore score (move, operation)
      | operation == 'S' = score + move
      | operation == 'U' = score * move
      | otherwise = score `div` move
    unvisited = boardMask .&. complement visited
    answer = sum
      [ score * fromIntegral (popCount (orthogonalNeighbors ! cell .&. unvisited))
      | (cell, score) <- zip path scores
      ]

showCell :: Int -> String
showCell cell = show (row + 1) ++ "," ++ show (column + 1)
  where
    (row, column) = cell `divMod` side

main :: IO ()
main = sequence_ $ take 1
  [ printWitness schedule witness
  | schedule <- arithmeticSchedules
  , witness <- solveSchedule schedule
  ]
