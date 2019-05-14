{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
  ( AttrMap
  , AttrName
  , Widget
  , attrMap
  , hBox
  , on
  , padAll
  , padBottom
  , padLeftRight
  , padTop
  , str
  , vBox
  , withAttr
  , withBorderStyle
  )
import Name (Name)

import Brick.Types (Padding(..))
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Constant (hGoGrid)
import Data.Maybe (isNothing)
import qualified Data.Vector.Unboxed as Vec
import qualified Graphics.Vty as V
import qualified Reducer as R
import Text.Printf (printf)

-- DRAWING
drawUI :: R.AppState -> [Widget Name]
drawUI g =
  case g of
    R.GameState {} -> appBox $ drawGame g
    R.Home mode -> appBox $ drawHome mode
    R.SoloSelectPlayer p -> appBox $ drawSoloSelectPlayer p
  where
    appBox w = [C.center $ withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Gomoku") w]

drawGame :: R.AppState -> Widget Name
drawGame R.GameState { R.goGrid = grd
                     , R.cursor = (cx, cy)
                     , R.cursorVisible = crv
                     , R.playerTurn = plTurn
                     , R.lastIATimeForPlay = lstTimCmp
                     , R.cursorSuggestion = sugCrd
                     , R.end = end
                     , R.nbPieceCapPBlack = nPCB
                     , R.nbPieceCapPWhite = nPCW
                     , R.nbTurn = nbTurn
                     } = hBox [padLeftRight 2 wInfo, padAll 2 wGoBoard, padLeftRight 2 wCmd]
    -- CMD
  where
    wCmd :: Widget Name
    wCmd =
      padAll 1 $
      vBox
        [ padTop (Pad 4) $ padBottom (Pad 1) $ str "Commands:"
        , str "Key Right: Right"
        , str "Key Up: Up"
        , str "Key Down: Down"
        , str "Key Left: Left"
        , padTop (Pad 1) $ str "ENTER: Pose a piece"
        , str "Key s: Suggestion play"
        , padTop (Pad 1) $ str "ESC: Quit the game"
        , str "Key q: Go menu"
        ]
    -- INFO
    wInfo :: Widget Name
    wInfo =
      padTop (Pad 5) $
      vBox
        ([padAll 1 $ vBox [str $ printf "Nb piece cap black: %d" nPCB, str $ printf "Nb piece cap white: %d" nPCW]] ++
         [padAll 1 $ str $ printf "Tour N %d" (div nbTurn 2)] ++
         [padAll 1 $ vBox endMsg] ++ [padAll 1 $ vBox [str "Time of last computation:", str $ printf "%f ms" lstTimCmp]])
    endMsg =
      case end of
        Nothing ->
          case plTurn of
            R.PlayerBlack -> [str "Player Turn: Black"]
            R.PlayerWhite -> [str "Player Turn: White"]
        Just mPl ->
          case mPl of
            Nothing -> [str "Match null"]
            Just plWin ->
              case plWin of
                R.PlayerWhite -> [str "Player White Win !"]
                R.PlayerBlack -> [str "Player Black Win !"]
    -- BOARD
    wGoBoard :: Widget Name
    wGoBoard = vBox $ [hBox $ map str boarderY] ++ map (cellsInRow grd) [0 .. hGoGrid - 1] ++ [hBox $ map str boarderY]
    boarderY = ["   "] ++ map (printf " %.2d") [0 .. 18 :: Int] ++ [" "]
    cellsInRow :: R.Grid -> Int -> Widget Name
    cellsInRow gd y =
      hBox $ [str $ printf "%.2d " y] ++ map (drawCell gd y) [0 .. hGoGrid - 1] ++ [str $ printf " %.2d" y]
    drawCell :: R.Grid -> Int -> Int -> Widget Name
    drawCell gr y x =
      if crv && isNothing end
        then if cx == x && cy == y
               then if not (R.validCoord grd plTurn (cx, cy))
                      then withAttr cursorBadAttr cw
                      else case R.playerToPiece plTurn of
                             R.PieceWhite -> withAttr pieceWhiteAttr cw
                             _ -> withAttr pieceBlackAttr cw
               else case sugCrd of
                      Just (csx, csy) ->
                        if csx == x && csy == y
                          then withAttr suggestionAttr cw
                          else cellToWiget $ R.charToCell $ gr Vec.! (y * hGoGrid + x)
                      Nothing -> cellToWiget $ R.charToCell $ gr Vec.! (y * hGoGrid + x)
        else cellToWiget $ R.charToCell $ gr Vec.! (y * hGoGrid + x)
    cellToWiget p =
      case p of
        R.PieceWhite -> withAttr pieceWhiteAttr $ str "(#)" -- "⚪  "
        R.PieceBlack -> withAttr pieceBlackAttr $ str "(#)" -- "⚫  "
        R.EmptyCell -> withAttr emptyAttr cw
    cw :: Widget Name
    cw = str "{#}" -- "() "
drawGame _ = str ""

drawHome :: R.GameMode -> Widget Name
drawHome mode = hBox wg
  where
    wg =
      case mode of
        R.GameSolo _ -> [padAll 1 $ withAttr selectedAttr $ str "Solo", padAll 1 $ str "Multi"]
        R.GameMulti -> [padAll 1 $ str "Solo", padAll 1 $ withAttr selectedAttr $ str "Multi"]

drawSoloSelectPlayer :: R.Player -> Widget Name
drawSoloSelectPlayer p = vBox [padAll 1 $ str "What do you want to play ?", hBox wg]
  where
    wg =
      case p of
        R.PlayerWhite -> [padAll 1 $ withAttr selectedAttr $ str "White (1st)", str "  ", padAll 1 $ str "Black (2nd)"]
        R.PlayerBlack -> [padAll 1 $ str "White (1st)", str "  ", padAll 1 $ withAttr selectedAttr $ str "Black (2nd)"]

-- ATTR MAP
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (pieceBlackAttr, V.black `on` V.yellow `V.withStyle` V.bold)
    , (pieceWhiteAttr, V.white `on` V.yellow `V.withStyle` V.bold)
    , (cursorBadAttr, V.red `on` V.yellow `V.withStyle` V.bold)
    , (suggestionAttr, V.cyan `on` V.yellow `V.withStyle` V.bold)
    , (emptyAttr, V.yellow `on` V.yellow)
    , (selectedAttr, V.black `on` V.white)
    ]

pieceBlackAttr :: AttrName
pieceBlackAttr = "gameOver"

pieceWhiteAttr :: AttrName
pieceWhiteAttr = "snakeAttr"

cursorBadAttr :: AttrName
cursorBadAttr = "cursorBadAttr"

emptyAttr :: AttrName
emptyAttr = "emptyAttr"

selectedAttr :: AttrName
selectedAttr = "selected"

suggestionAttr :: AttrName
suggestionAttr = "suggestion"