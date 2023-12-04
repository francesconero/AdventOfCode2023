import Data.List
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Card = Card
  { cardId :: !Int,
    winningNumbers :: ![Int],
    sampledNumbers :: ![Int]
  }
  deriving (Show)

cardParser = do
  id <- string "Card" *> spaces *> nat <* char ':' <* spaces
  winning <- sepEndBy nat spaces
  char '|' *> spaces
  sampled <- sepEndBy nat spaces
  return (Card id winning sampled)

points :: Card -> Int
points card = case length $ intersect (winningNumbers card) (sampledNumbers card) of
  n
    | n < 1 -> 0
    | otherwise -> 2 ^ (n - 1)

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      let parsedCards = traverse (parse cardParser "") $ lines contents
      case parsedCards of
        Right cards -> print $ sum $ map points cards
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"