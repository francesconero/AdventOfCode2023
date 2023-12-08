import Data.List
import Data.Maybe
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Hand = Hand
  { cards :: !String,
    bet :: !Int
  }
  deriving (Show)

parseHand = do
  cards <- many alphaNum
  space
  bet <- nat
  return $ Hand cards bet

parseAll = sepEndBy parseHand spaces

distinctCounts :: Hand -> [Int]
distinctCounts hand = sort $ map length . group . sort $ cards hand

jokers :: Hand -> Int
jokers hand = length $ filter (== 'J') (cards hand)

scoreHand :: Hand -> Int
scoreHand hand
  | jokerCount == 0 && distinctCardCounts == [5] = 7
  | jokerCount == 0 && distinctCardCounts == [1, 4] = 6
  | jokerCount == 0 && distinctCardCounts == [2, 3] = 5
  | jokerCount == 0 && distinctCardCounts == [1, 1, 3] = 4
  | jokerCount == 0 && distinctCardCounts == [1, 2, 2] = 3
  | jokerCount == 0 && distinctCardCounts == [1, 1, 1, 2] = 2
  | jokerCount == 0 && distinctCardCounts == [1, 1, 1, 1, 1] = 1
  | jokerCount == 5 && distinctCardCounts == [5] = 7
  | jokerCount == 1 && distinctCardCounts == [1, 4] = 7
  | jokerCount == 4 && distinctCardCounts == [1, 4] = 7
  | jokerCount == 2 && distinctCardCounts == [2, 3] = 7
  | jokerCount == 3 && distinctCardCounts == [2, 3] = 7
  | jokerCount == 1 && distinctCardCounts == [1, 1, 3] = 6
  | jokerCount == 3 && distinctCardCounts == [1, 1, 3] = 6
  | jokerCount == 1 && distinctCardCounts == [1, 2, 2] = 5
  | jokerCount == 2 && distinctCardCounts == [1, 2, 2] = 6
  | jokerCount == 1 && distinctCardCounts == [1, 1, 1, 2] = 4
  | jokerCount == 2 && distinctCardCounts == [1, 1, 1, 2] = 4
  | jokerCount == 1 && distinctCardCounts == [1, 1, 1, 1, 1] = 2
  where
    jokerCount = jokers hand
    distinctCardCounts = distinctCounts hand

scoreCard :: Char -> Int
scoreCard 'A' = 12
scoreCard 'K' = 11
scoreCard 'Q' = 10
scoreCard 'T' = 9
scoreCard '9' = 8
scoreCard '8' = 7
scoreCard '7' = 6
scoreCard '6' = 5
scoreCard '5' = 4
scoreCard '4' = 3
scoreCard '3' = 2
scoreCard '2' = 1
scoreCard 'J' = 0

scoreCards :: Hand -> Int
scoreCards hand = foldl' (\acc curr -> 13 * acc + scoreCard curr) 0 (cards hand)

compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2
  | hand1Score > hand2Score = GT
  | hand1Score < hand2Score = LT
  | hand1Score == hand2Score = if scoreCards hand1 > scoreCards hand2 then GT else LT
  where
    hand1Score = scoreHand hand1
    hand2Score = scoreHand hand2

scoreHands :: [Hand] -> Int
scoreHands hands = fst $ foldl' (\(out, i) (Hand card bet) -> (i * bet + out, i + 1)) (0, 1) hands

finalScore :: [Hand] -> Int
finalScore hands = scoreHands $ sortBy compareHands hands

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right hands -> print . finalScore $ hands
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
