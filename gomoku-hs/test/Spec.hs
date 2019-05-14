import qualified Reducer as R

grdEmpty :: [[Cell]]
grdEmpty = [[R.EmptyCell | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
