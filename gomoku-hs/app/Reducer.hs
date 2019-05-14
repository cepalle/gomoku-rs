module Reducer where

import Constant (allDir, allDirVb, gridInitVb, hGoGrid)
import Control.DeepSeq
import Data.List (foldl', sortBy)
import qualified Data.Vector as Vb
import qualified Data.Vector.Unboxed as Vec

-- import Control.Parallel (par)
-- import Data.List (elemIndex)
-- import Data.List.Split (chunksOf)
import Data.Maybe (isJust)

import System.CPUTime

-- import System.IO
-- import System.Random (Random(..), newStdGen)
-- TYPES STATE
data AppState
  = GameState { goGrid :: Grid -- go grid with piece
              , gameMode :: GameMode -- solo or tow player
              , playerTurn :: Player -- turn of player
              , lastIATimeForPlay :: Double -- time for IA play
              , cursorSuggestion :: Maybe Coord -- suggestion IA
              , cursor :: Coord -- user cursor for play
              , cursorVisible :: Bool -- cursor visibility alternator
              , end :: Maybe (Maybe Player) -- Maybe contain player win or Nothing if match null
              , nbPieceCapPWhite :: Int -- nombre de piece capture by player white
              , nbPieceCapPBlack :: Int -- nombre de piece capture by player black
              , nbTurn :: Int }
  | Home GameMode
  | SoloSelectPlayer Player
  deriving (Eq, Show)

data Cell
  = PieceBlack
  | PieceWhite
  | EmptyCell
  deriving (Eq, Show)

data Player
  = PlayerWhite
  | PlayerBlack
  deriving (Eq, Show)

data GameMode
  = GameSolo Player
  | GameMulti
  deriving (Eq, Show)

type Coord = (Int, Int)

type Grid = Vec.Vector Char -- length 19 * 19

type GridBool = Vec.Vector Bool -- length 19 * 19

-- INIT STATE
initState :: AppState
initState = Home (GameSolo PlayerWhite)

initGameState :: GameMode -> AppState
initGameState mode =
  let grid = Vec.replicate (hGoGrid * hGoGrid) $ cellToChar EmptyCell
   in GameState
        { goGrid =
            case mode of
              GameSolo PlayerBlack ->
                Vec.imap
                  (\i e ->
                     if i == hGoGrid * 8 + 8
                       then cellToChar PieceWhite
                       else e)
                  grid
              _ -> grid
        , gameMode = mode
        , playerTurn =
            case mode of
              GameSolo PlayerBlack -> PlayerBlack
              _ -> PlayerWhite
        , lastIATimeForPlay = 0.0
        , cursorSuggestion = Nothing
        , cursor = (9, 9)
        , cursorVisible = True
        , end = Nothing
        , nbPieceCapPBlack = 0
        , nbPieceCapPWhite = 0
        , nbTurn =
            case mode of
              GameSolo PlayerBlack -> 3
              _ -> 2
        }

-- UPDATE STATE
data CursorDir
  = CursorUp
  | CursorDown
  | CursorRight
  | CursorLeft

moveCursor :: AppState -> CursorDir -> (Int, Int)
moveCursor GameState {cursor = (x, y)} d =
  case d of
    CursorUp -> (x, (y - 1) `mod` hGoGrid)
    CursorDown -> (x, (y + 1) `mod` hGoGrid)
    CursorRight -> ((x + 1) `mod` hGoGrid, y)
    CursorLeft -> ((x - 1) `mod` hGoGrid, y)
moveCursor _ _ = (0, 0)

handelPlayCoord :: Coord -> AppState -> AppState
handelPlayCoord cr s =
  case end s of
    Nothing ->
      if validCoord (goGrid s) (playerTurn s) cr
        then let nwS = checkEnd cr $ checkCaptur cr $ s {goGrid = posePiece cr (playerTurn s) (goGrid s)}
              in nwS {playerTurn = nextPlayer (playerTurn nwS), nbTurn = 1 + nbTurn nwS}
        else s
    _ -> s

posePiece :: Coord -> Player -> Grid -> Grid
posePiece (cx, cy) p grid = Vec.imap putPiece grid
  where
    putPiece :: Int -> Char -> Char
    putPiece idx c =
      if idx == hGoGrid * cy + cx
        then playerToChar p
        else c

posePieceAndDelete :: Coord -> Player -> Grid -> (Grid, Int)
posePieceAndDelete cr p grd =
  let withPiece :: Grid
      withPiece = posePiece cr p grd
      toSup :: Vb.Vector (Vb.Vector (Int, Int, Player))
      toSup = checkCapturToSup p cr withPiece
      newGrd = supPosGrid withPiece toSup
   in (newGrd, 2 * Vb.length toSup)

checkCaptur :: Coord -> AppState -> AppState
checkCaptur cr s =
  let toSup = checkCapturToSup (playerTurn s) cr (goGrid s)
      nbCap = length toSup * 2
      newGrd = supPosGrid (goGrid s) toSup
   in case playerTurn s of
        PlayerBlack -> s {goGrid = newGrd, nbPieceCapPBlack = nbPieceCapPBlack s + nbCap}
        PlayerWhite -> s {goGrid = newGrd, nbPieceCapPWhite = nbPieceCapPWhite s + nbCap}

memoCapturToSup ::
     (Vb.Vector (Vb.Vector (Vb.Vector (Int, Int, Player))), Vb.Vector (Vb.Vector (Vb.Vector (Int, Int, Player))))
memoCapturToSup =
  let mw = Vb.imap (\i _ -> Vb.map (genPosCheck (mod i hGoGrid, div i hGoGrid) PlayerWhite) allDirVb) gridInitVb
      mb = Vb.imap (\i _ -> Vb.map (genPosCheck (mod i hGoGrid, div i hGoGrid) PlayerBlack) allDirVb) gridInitVb
   in (mw, mb)
  where
    genPosCheck (cx, cy) player (dx, dy) =
      Vb.fromList
        [ (cx + dx, cy + dy, nextPlayer player)
        , (cx + dx * 2, cy + dy * 2, nextPlayer player)
        , (cx + dx * 3, cy + dy * 3, player)
        ]

checkCapturToSup :: Player -> Coord -> Grid -> Vb.Vector (Vb.Vector (Int, Int, Player))
checkCapturToSup p (cx, cy) grd =
  let (mw, mn) = memoCapturToSup
      toCheck :: Vb.Vector (Vb.Vector (Vb.Vector (Int, Int, Player)))
      toCheck =
        if p == PlayerWhite
          then mw
          else mn
   in Vb.filter (checkPoss grd) $ toCheck Vb.! (cy * hGoGrid + cx)
  where
    checkPoss :: Grid -> Vb.Vector (Int, Int, Player) -> Bool
    checkPoss grid psCks = Vb.length (Vb.filter (checkPos grid) psCks) == 3
    checkPos :: Grid -> (Int, Int, Player) -> Bool
    checkPos gd (x, y, plr) =
      x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && gd Vec.! (hGoGrid * y + x) == playerToChar plr

supPosGrid :: Grid -> Vb.Vector (Vb.Vector (Int, Int, Player)) -> Grid
supPosGrid grd toSup = foldl' supElGrd grd toSup
  where
    supElGrd :: Grid -> Vb.Vector (Int, Int, Player) -> Grid
    supElGrd gd poss =
      let (fx, fy, _) = Vb.head poss
          (sx, sy, _) = poss Vb.! 1
       in Vec.imap
            (\i e ->
               if i == (fy * hGoGrid + fx) || i == (sy * hGoGrid + sx)
                 then cellToChar EmptyCell
                 else e)
            gd

handelIAPlay :: AppState -> IO AppState
handelIAPlay s = do
  start <- getCPUTime
  let mCoord = solver (goGrid s) (playerTurn s) (nbPieceCapPWhite s) (nbPieceCapPBlack s)
  endTimer <- mCoord `deepseq` getCPUTime
  let diff = fromIntegral (endTimer - start) / (10 ^ 9)
  let withDiff = s {lastIATimeForPlay = diff}
  return (handelPlayCoord mCoord withDiff)

suggestionPlay :: AppState -> IO AppState
suggestionPlay s =
  if isJust (end s)
    then return s
    else do
      start <- getCPUTime
      let coord = solver (goGrid s) (playerTurn s) (nbPieceCapPWhite s) (nbPieceCapPBlack s)
      endTimer <- coord `deepseq` getCPUTime
      let diff = fromIntegral (endTimer - start) / (10 ^ 9)
      return s {lastIATimeForPlay = diff, cursorSuggestion = Just coord}

-- UTIL
playerToPiece :: Player -> Cell
playerToPiece PlayerWhite = PieceWhite
playerToPiece PlayerBlack = PieceBlack

nextPlayer :: Player -> Player
nextPlayer PlayerWhite = PlayerBlack
nextPlayer PlayerBlack = PlayerWhite

playerToChar :: Player -> Char
playerToChar PlayerWhite = '1'
playerToChar PlayerBlack = '2'

cellToChar :: Cell -> Char
cellToChar EmptyCell = '0'
cellToChar PieceWhite = '1'
cellToChar PieceBlack = '2'

charToCell :: Char -> Cell
charToCell '0' = EmptyCell
charToCell '1' = PieceWhite
charToCell '2' = PieceBlack
charToCell _ = EmptyCell

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

validCoords :: Grid -> Player -> GridBool
validCoords grd p = delDoubleThree grd p (Vec.map (== cellToChar EmptyCell) grd)

validCoord :: Grid -> Player -> Coord -> Bool
validCoord grd p (cx, cy) =
  cx >= 0 && cx < hGoGrid && cy >= 0 && cy < hGoGrid && validCoords grd p Vec.! (cy * hGoGrid + cx)

validCoordToList :: GridBool -> [(Int, Int)]
validCoordToList grid = [(x, y) | x <- [0 .. hGoGrid - 1], y <- [0 .. hGoGrid - 1], grid Vec.! (y * hGoGrid + x)]

checkEnd :: Coord -> AppState -> AppState
checkEnd cr s
  | nbPieceCapPWhite s >= 10 = s {end = Just (Just PlayerWhite)}
  | nbPieceCapPBlack s >= 10 = s {end = Just (Just PlayerBlack)}
  | checkAlign5 cr (goGrid s) (playerTurn s) = s {end = Just (Just (playerTurn s))}
  | 0 == Vec.length (Vec.filter id $ validCoords (goGrid s) (playerTurn s)) = s {end = Just Nothing}
  | otherwise = s
  where
    checkAlign5 :: Coord -> Grid -> Player -> Bool
    checkAlign5 c grd p = checkAllPos grd p $ allDir >>= genPosCheck c
    maskCf :: [[Int]]
    maskCf = [[-4, -3, -2, -1, 0], [-3, -2, -1, 0, 1], [-2, -1, 0, 1, 2]]
    genPosCheck :: Coord -> Coord -> [[Coord]]
    genPosCheck (cx, cy) (dx, dy) = map (map (\k -> (cx + dx * k, cy + dy * k))) maskCf
    checkAllPos :: Grid -> Player -> [[Coord]] -> Bool
    checkAllPos grd p lpos =
      let tmp = map (length . filter (checkPos grd p)) lpos
       in (0 /= length (filter (== 5) tmp))
    checkPos :: Grid -> Player -> Coord -> Bool
    checkPos grd p (x, y) =
      x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && grd Vec.! (y * hGoGrid + x) == playerToChar p

------------
-- SOLVER --
------------
maskCoef :: Cell -> [[(Int, Cell)]]
maskCoef pc =
  [ [(-3, EmptyCell), (-2, pc), (-1, pc), (0, EmptyCell), (1, EmptyCell)]
  , [(-2, EmptyCell), (-1, pc), (0, EmptyCell), (1, pc), (2, EmptyCell)]
  , [(-4, EmptyCell), (-3, pc), (-2, pc), (-1, EmptyCell), (0, EmptyCell), (1, EmptyCell)]
  , [(-2, EmptyCell), (-1, pc), (0, EmptyCell), (1, EmptyCell), (2, pc), (3, EmptyCell)]
  , [(-1, EmptyCell), (0, EmptyCell), (1, pc), (2, EmptyCell), (3, pc), (4, EmptyCell)]
  ]

memoMaskWhite :: [[(Int, Cell)]]
memoMaskWhite = maskCoef $ playerToPiece PlayerWhite

memoMaskBlack :: [[(Int, Cell)]]
memoMaskBlack = maskCoef $ playerToPiece PlayerBlack

memoDoubleThree ::
     ( Vb.Vector (Vb.Vector (Vb.Vector (Int, Int, Cell), (Int, Int)))
     , Vb.Vector (Vb.Vector (Vb.Vector (Int, Int, Cell), (Int, Int))))
memoDoubleThree =
  let genWhite =
        Vb.imap (\i _ -> Vb.fromList $ allDir >>= genPosCheck memoMaskWhite (mod i hGoGrid, div i hGoGrid)) gridInitVb
      genBlack =
        Vb.imap (\i _ -> Vb.fromList $ allDir >>= genPosCheck memoMaskBlack (mod i hGoGrid, div i hGoGrid)) gridInitVb
   in (genWhite, genBlack)
  where
    genPosCheck :: [[(Int, Cell)]] -> Coord -> Coord -> [(Vb.Vector (Int, Int, Cell), (Int, Int))]
    genPosCheck msk (cx, cy) (dx, dy) =
      map (\r -> (Vb.fromList $ map (\(k, c) -> (cx + dx * k, cy + dy * k, c)) r, (dx, dy))) msk

delDoubleThree :: Grid -> Player -> GridBool -> GridBool
delDoubleThree grd p grd_old =
  let (mw, mn) = memoDoubleThree
      toCheck =
        if p == PlayerWhite
          then mw
          else mn
   in Vec.imap (\i e -> e && checkAllPos grd (toCheck Vb.! i)) grd_old
  where
    checkAllPos :: Grid -> Vb.Vector (Vb.Vector (Int, Int, Cell), (Int, Int)) -> Bool
    checkAllPos grida lpos =
      let tmp = Vb.map snd $ Vb.filter (checkLPos grida) lpos
          dDir = Vb.foldl' delDir [] tmp
       in 1 >= length dDir
    checkLPos :: Grid -> (Vb.Vector (Int, Int, Cell), (Int, Int)) -> Bool
    checkLPos grd' (lp, _) = Vb.length lp == Vb.length (Vb.filter (checkPos grd') lp)
    checkPos :: Grid -> (Int, Int, Cell) -> Bool
    checkPos grid (x, y, pc) =
      x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && grid Vec.! (y * hGoGrid + x) == cellToChar pc
    delDir :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    delDir acc (drx, dry) = filter (\(dx, dy) -> not (drx == negate dx && dry == negate dy)) $ acc ++ [(drx, dry)]

-- True if is dist <= maxDist
distEmptyCellMap :: Grid -> Int -> GridBool
distEmptyCellMap grille maxDist =
  let initMap = Vec.map (== cellToChar EmptyCell) grille
      iterator = [1 .. maxDist]
   in Vec.map not $ foldl' (\b _ -> addDist1 b) initMap iterator
  where
    addDist1 :: GridBool -> GridBool
    addDist1 grid = Vec.imap (\i e -> e && not (checkNeighbour grid (mod i hGoGrid) (div i hGoGrid))) grid
    checkNeighbour :: GridBool -> Int -> Int -> Bool
    checkNeighbour grd x y =
      checkPos grd (x + 1) y ||
      checkPos grd x (y + 1) ||
      checkPos grd x (y - 1) ||
      checkPos grd (x - 1) y ||
      checkPos grd (x + 1) (y + 1) ||
      checkPos grd (x - 1) (y + 1) || checkPos grd (x + 1) (y - 1) || checkPos grd (x - 1) (y - 1)
    checkPos :: GridBool -> Int -> Int -> Bool
    checkPos gd x y = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && not (gd Vec.! (y * hGoGrid + x))

-- Socring Ordoring
countDir :: Grid -> Player -> Coord -> (Int, Int) -> Int
countDir grid player cr (dx, dy) =
  let (_, c, be) = foldl' (sumDist grid player cr (dx, dy)) (True, 0, False) [1 .. 4]
      (_, c', be') = foldl' (sumDist grid player cr (-dx, -dy)) (True, 0, False) [1 .. 4]
   in c + c' + 1 + (boolToInt be) + (boolToInt be')

sumDist :: Grid -> Player -> Coord -> (Int, Int) -> (Bool, Int, Bool) -> Int -> (Bool, Int, Bool)
sumDist grid player (cx, cy) (dx, dy) (b, nb, _) d =
  let (cx', cy') = (cx + d * dx, cy + d * dy)
   in if b && 0 <= cx' && 0 <= cy' && hGoGrid > cx' && hGoGrid > cy'
        then if grid Vec.! (cy' * hGoGrid + cx') /= playerToChar player
               then if grid Vec.! (cy' * hGoGrid + cx') == cellToChar EmptyCell
                      then (False, nb, True)
                      else (False, nb, False)
               else (True, nb + 1, False)
        else (False, nb, False)

moveScoringAlign :: Grid -> Player -> Coord -> Int
moveScoringAlign grid player move =
  let countedDir = map (countDir grid player move) [(0, 1), (1, 0), (1, 1), (1, -1)]
      score = foldl' (\p c -> p + countToScore c) 0 countedDir
   in score

moveScoringCap :: Grid -> Int -> Int -> Player -> Coord -> (Int, Int, Int)
moveScoringCap grid capWhite capBlack player move =
  let newCap = 2 * Vb.length (checkCapturToSup player move grid)
      nbCap =
        if player == PlayerWhite
          then capWhite + newCap
          else capBlack + newCap
      scoreCapture =
        if nbCap >= 10
          then scoreEndGame
          else scoreCap * newCap
   in if player == PlayerWhite
        then (scoreCapture, nbCap, capBlack)
        else (scoreCapture, capWhite, nbCap)

scoreEndGame :: Int
scoreEndGame = 10000000

cutNegaMax :: Int
cutNegaMax = div scoreEndGame 2

depthScoreOffset :: Int
depthScoreOffset = 100000

scoreCap :: Int
scoreCap = 555

countToScore :: Int -> Int
countToScore count
  | count == 2 = 10
  | count == 3 = 100
  | count == 4 = 10000
  | count == 5 = scoreEndGame
  | otherwise = 0

scoringOrdoring :: Grid -> Int -> Int -> Player -> Coord -> Int
scoringOrdoring grid capWhite capBlack player move =
  let sp = moveScoringAlign grid player move
      (sc, _, _) = moveScoringCap grid capWhite capBlack player move
      so = moveScoringAlign grid (nextPlayer player) move
      (sco, _, _) = moveScoringCap grid capWhite capBlack (nextPlayer player) move
   in sp + so + sc + sco

-- Scoring End
scoreLine :: Grid -> Player -> Coord -> (Int, Int) -> Int
scoreLine grd p (cx, cy) (dx, dy) =
  let (s, c) = foldl' (\acc i -> foldlScoreLine grd p (cx + dx * i, cy + dy * i) acc) (0, 0) [0 .. hGoGrid - 1]
   in s + countToScore c

foldlScoreLine :: Grid -> Player -> Coord -> (Int, Int) -> (Int, Int)
foldlScoreLine grd p (cx, cy) (sAcc, cur) =
  if cx >= 0 && cy >= 0 && cx < hGoGrid && cy < hGoGrid && grd Vec.! (cx + cy * hGoGrid) == playerToChar p
    then (sAcc, cur + 1)
    else (sAcc + countToScore cur, 0)

scoreAlignY :: Grid -> Player -> Int
scoreAlignY grid player = foldl' (+) 0 $ map (\i -> scoreLine grid player (0, i) (1, 0)) [0 .. hGoGrid - 1]

scoreAlignX :: Grid -> Player -> Int
scoreAlignX grid player = foldl' (+) 0 $ map (\i -> scoreLine grid player (i, 0) (0, 1)) [0 .. hGoGrid - 1]

scoreAlignDiag2 :: Grid -> Player -> Int
scoreAlignDiag2 grid player =
  let s1 = foldl' (+) 0 $ map (\i -> scoreLine grid player (0, i) (1, 1)) [0 .. hGoGrid - 1]
      s2 = foldl' (+) 0 $ map (\i -> scoreLine grid player (i, 0) (1, 1)) [1 .. hGoGrid - 1]
   in s1 + s2

scoreAlignDiag1 :: Grid -> Player -> Int
scoreAlignDiag1 grid player =
  let s1 = foldl' (+) 0 $ map (\i -> scoreLine grid player (i, 0) (-1, 1)) [0 .. hGoGrid - 1]
      s2 = foldl' (+) 0 $ map (\i -> scoreLine grid player (hGoGrid - 1, i) (-1, 1)) [1 .. hGoGrid - 1]
   in s1 + s2

scoreAlign :: Grid -> Player -> Int
scoreAlign grid player =
  scoreAlignDiag1 grid player + scoreAlignDiag2 grid player + scoreAlignX grid player + scoreAlignY grid player

scoringEnd :: Grid -> Int -> Int -> Player -> Int
scoringEnd grid capWhite capBlack player =
  let scoreCapBlack =
        if capBlack >= 10
          then scoreEndGame
          else capBlack * scoreCap
      scoreCapWhite =
        if capWhite >= 10
          then scoreEndGame
          else capWhite * scoreCap
      scoreAlignBlack = scoreAlign grid PlayerBlack
      scoreAlignWhite = scoreAlign grid PlayerWhite
   in if player == PlayerWhite
        then scoreCapWhite + scoreAlignWhite - scoreCapBlack - (scoreAlignBlack * 2)
        else scoreCapBlack + scoreAlignBlack - scoreCapWhite - (scoreAlignWhite * 2)

-- SOLVER
solver :: Grid -> Player -> Int -> Int -> Coord
solver = miniWrapper

-- /!\ no valide play if the map is Empty!
nextMoves :: Grid -> [Coord]
nextMoves grid = validCoordToList $ validIACoords grid 1

validIACoords :: Grid -> Int -> GridBool
validIACoords grd d =
  let empty = Vec.map (== cellToChar EmptyCell) grd
      grd_dist = distEmptyCellMap grd d
      emptyAndDist = Vec.imap (\i e -> e && grd_dist Vec.! i) empty
   in emptyAndDist

compF :: (Coord, Int) -> (Coord, Int) -> Ordering
compF (_, s1) (_, s2)
  | s1 > s2 = LT
  | s1 < s2 = GT
  | otherwise = EQ

negaMax :: Grid -> Player -> Int -> Int -> Int -> Int -> Int -> Int
negaMax grid player depth alpha beta capWhite capBlack =
  let moves = nextMovesFirst grid player
      nxtMovesAndScore :: [(Coord, Int)]
      nxtMovesAndScore = map (\(cx, cy) -> ((cx, cy), scoringOrdoring grid capWhite capBlack player (cx, cy))) moves
      movesSort = sortBy compF nxtMovesAndScore
      abPruning a (cr, _) =
        if a >= beta || a >= cutNegaMax
          then a
          else let (newGrid, nbDel) = posePieceAndDelete cr player grid
                   nW =
                     if player == PlayerWhite
                       then capWhite + nbDel
                       else capWhite
                   nB =
                     if player == PlayerBlack
                       then capBlack + nbDel
                       else capBlack
                   resNega = negate $ negaMax newGrid (nextPlayer player) (depth - 1) (-beta) (-a) nW nB
                   newAlpha = max a resNega
                in newAlpha
      res =
        if depth > 0
          then foldl' abPruning alpha $ take 9 movesSort
          else scoringEnd grid capWhite capBlack player
   in res

-- Wrapper
nextMovesFirst :: Grid -> Player -> [Coord]
nextMovesFirst grid player = validCoordToList $ validIACoordsFirst grid player 1

validIACoordsFirst :: Grid -> Player -> Int -> GridBool
validIACoordsFirst grd p d =
  let empty = Vec.map (== cellToChar EmptyCell) grd
      grd_dist = distEmptyCellMap grd d
      emptyAndDist = Vec.imap (\i e -> e && grd_dist Vec.! i) empty
      v = delDoubleThree grd p emptyAndDist
   in v

miniWrapper :: Grid -> Player -> Int -> Int -> Coord
miniWrapper grid player capWhite capBlack =
  let depth = 5
      alpha = div (minBound :: Int) 8
      beta = div (maxBound :: Int) 8
      moves = nextMovesFirst grid player
      nxtMovesAndScore :: [(Coord, Int)]
      nxtMovesAndScore = map (\(cx, cy) -> ((cx, cy), scoringOrdoring grid capWhite capBlack player (cx, cy))) moves
      movesSort = sortBy compF nxtMovesAndScore
      abPruning (a, co) (cr, _) =
        if a >= beta || a >= cutNegaMax
          then (a, co)
          else let (newGrid, nbDel) = posePieceAndDelete cr player grid
                   nW =
                     if player == PlayerWhite
                       then capWhite + nbDel
                       else capWhite
                   nB =
                     if player == PlayerBlack
                       then capBlack + nbDel
                       else capBlack
                   resNega = negate $ negaMax newGrid (nextPlayer player) (depth - 1) (-beta) (-a) nW nB
                in if resNega > a || resNega > cutNegaMax
                     then (resNega, cr)
                     else (a, co)
      (_, bestMove) = foldl' abPruning (alpha, (8, 8)) $ take 32 movesSort
   in bestMove
