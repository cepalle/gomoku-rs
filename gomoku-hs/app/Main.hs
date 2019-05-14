module Main
  ( main
  ) where

import Brick.BChan (newBChan, writeBChan)
import Brick.Main (App(..), customMain, neverShowCursor)
import Control.Concurrent.Extra (forkIO, threadDelay)
import Control.Monad.Extra (forever, void)
import Name (Name)

import qualified Event as E
import qualified Graphics.Vty as V
import qualified Reducer as R
import qualified UI as U

-- MAIN APP
app :: App R.AppState E.CustomEvent Name
app =
  App
    { appDraw = U.drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = E.handleEvent
    , appStartEvent = return
    , appAttrMap = const U.theMap
    }

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $
    forever $ do
      writeBChan chan E.Tick
      threadDelay 300000 -- cursor alternator speed
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app R.initState
