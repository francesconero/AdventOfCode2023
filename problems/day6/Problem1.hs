import Data.List
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Game = Game
  { allowedTime :: !Int,
    distanceRecord :: !Int
  }
  deriving (Show)

solve :: Game -> (Int, Int)
solve game = (lowerSolution, higherSolution)
  where
    t = fromIntegral (allowedTime game)
    d = fromIntegral (distanceRecord game)
    discriminantSqrt = sqrt (t ^ 2 - 4 * d)
    x1 = (t - discriminantSqrt) / 2
    x2 = (t + discriminantSqrt) / 2
    lowerSolution = if x1 == fromIntegral (ceiling x1) then ceiling x1 + 1 else ceiling x1
    higherSolution = if x2 == fromIntegral (floor x2) then floor x2 - 1 else floor x2

productFold :: [Int] -> Int
productFold = foldl' (*) 1

rangeSize :: (Int, Int) -> Int
rangeSize (a, b) = b - a + 1

parseAll = do
  times <- string "Time:" *> spaces *> sepEndBy nat spaces
  distances <- string "Distance:" *> spaces *> sepEndBy nat spaces
  return $ productFold $ map (rangeSize . solve) (zipWith Game times distances)

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right parsed -> print parsed
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
