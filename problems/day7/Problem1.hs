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

scoreHand :: Hand -> Int
scoreHand hand
  | distinctCounts hand == [5] = 7
  | distinctCounts hand == [1, 4] = 6
  | distinctCounts hand == [2, 3] = 5
  | distinctCounts hand == [1, 1, 3] = 4
  | distinctCounts hand == [1, 2, 2] = 3
  | distinctCounts hand == [1, 1, 1, 2] = 2
  | distinctCounts hand == [1, 1, 1, 1, 1] = 1

scoreCard :: Char -> Int
scoreCard 'A' = 12
scoreCard 'K' = 11
scoreCard 'Q' = 10
scoreCard 'J' = 9
scoreCard 'T' = 8
scoreCard '9' = 7
scoreCard '8' = 6
scoreCard '7' = 5
scoreCard '6' = 4
scoreCard '5' = 3
scoreCard '4' = 2
scoreCard '3' = 1
scoreCard '2' = 0

scoreCards :: Hand -> Int
scoreCards hand = foldl' (\acc curr -> 13 * acc + scoreCard curr) 0 (cards hand)

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
