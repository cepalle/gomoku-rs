module Constant where

import qualified Data.Vector as Vb

hGoGrid :: Int
hGoGrid = 19

allDir :: [(Int, Int)]
allDir = [(0, 1), (1, 0), (1, 1), (-1, -1), (-1, 0), (0, -1), (-1, 1), (1, -1)]

allDirVb :: Vb.Vector (Int, Int)
allDirVb = Vb.fromList allDir

gridInitVb :: Vb.Vector Bool
gridInitVb = Vb.replicate (hGoGrid * hGoGrid) True