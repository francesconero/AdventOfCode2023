import Data.Foldable
import Data.Functor
import Data.List as List
import Data.Map as Map
import Data.Maybe
import qualified Data.Ord as List
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number
import Text.Printf

data Direction = U | R | D | L deriving (Show, Eq)

data RGB = RGB {red :: Int, green :: Int, blue :: Int} deriving (Show, Eq)

data Instruction = Instruction {direction :: Direction, steps :: Int} deriving (Show, Eq)

newtype Point = Point (Int, Int) deriving (Show, Eq)

newtype Segment = Segment (Point, Point) deriving (Show, Eq)

parseDirection :: Parser Direction
parseDirection = do
  direction <- nat
  return $ case direction of
    3 -> U
    0 -> R
    1 -> D
    2 -> L

parseHex :: Int -> Parser Int
parseHex n = do
  hex <- count n hexDigit
  case parse (try hexnum) "" hex of
    Left err -> fail (show err)
    Right val -> return val

parseInstruction :: Parser Instruction
parseInstruction = do
  manyTill anyChar (char '#')
  steps <- parseHex 5
  direction <- parseDirection
  char ')'
  return $ Instruction direction steps

parseAll = sepEndBy1 parseInstruction (char '\n')

dig :: Point -> Instruction -> Segment
dig (Point (x, y)) (Instruction direction steps) = Segment (Point (startX, startY), Point (endX, endY))
  where
    ((startX, startY), (endX, endY)) = case direction of
      U -> ((x, y), (x, y - steps))
      R -> ((x, y), (x + steps, y))
      D -> ((x, y), (x, y + steps))
      L -> ((x, y), (x - steps, y))

digAll :: [Segment] -> [Instruction] -> [Segment]
digAll segments [] = segments
digAll [] (i : is) = digAll [dig (Point (0, 0)) i] is
digAll (Segment (p1, p2) : ss) (i : is) = digAll (dig p2 i : Segment (p1, p2) : ss) is

shoelaceFormula :: [Segment] -> Int
shoelaceFormula segments = abs $ List.foldr addDeterminant 0 segments `div` 2
  where
    addDeterminant :: Segment -> Int -> Int
    addDeterminant (Segment (Point (x1, y1), Point (x2, y2))) acc = acc + (x1 * y2) - (x2 * y1)

boundaryLength :: [Instruction] -> Int
boundaryLength = List.foldl' (\acc (Instruction _ steps) -> acc + steps) 4

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right parsed -> do
          let dug = digAll [] parsed
          let perimeter = boundaryLength parsed
          let innerArea = shoelaceFormula (reverse dug)
          let area = innerArea + (perimeter `div` 2) - 1
          print area
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
