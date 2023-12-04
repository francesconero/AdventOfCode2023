import Data.List
import Debug.Trace
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

matches :: Card -> Int
matches card = length $ intersect (winningNumbers card) (sampledNumbers card)

points :: Card -> Int
points card = case matches card of
  n
    | n < 1 -> 0
    | otherwise -> 2 ^ (n - 1)

addKToNElements :: Int -> Int -> [Int] -> [Int]
addKToNElements k n xs =
  let (increased, remaining) = foldl' (\(output, toAdd) current -> (output ++ [current + k * min 1 (max toAdd 0)], toAdd - 1)) ([], n) xs
   in (increased ++ replicate remaining k)

soManyCards :: (Int, [Int]) -> Card -> (Int, [Int])
soManyCards (total, cardsWon) card = case cardsWon of
  [] -> (total + 1, addKToNElements 1 (matches card) [])
  current : rest -> (total + current + 1, addKToNElements (current + 1) (matches card) rest)

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      let parsedCards = traverse (parse cardParser "") $ lines contents
      case parsedCards of
        Right cards -> print $ fst $ foldl' soManyCards (0, []) cards
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
