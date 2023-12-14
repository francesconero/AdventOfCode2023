import Data.List
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Tile = EmptyTile | CircleRock | SquareRock deriving (Show, Eq)

parseTile :: Parser Tile
parseTile = do
  pos <- getPosition
  c <- oneOf ".#O"
  return $ case c of
    '.' -> EmptyTile
    'O' -> CircleRock
    '#' -> SquareRock

parseField :: Parser [[Tile]]
parseField = sepEndBy (many1 parseTile) endOfLine

parseAll :: Parser [[Tile]]
parseAll = parseField

rotateField :: [[Tile]] -> [[Tile]]
rotateField = transpose . reverse

fieldToString :: [[Tile]] -> String
fieldToString field =
  intercalate "\n" $
    map
      ( concatMap
          ( \tile -> case tile of
              EmptyTile -> "."
              CircleRock -> "O"
              SquareRock -> "#"
          )
      )
      field

sumWeights :: Int -> Int -> Int -> Int
sumWeights 0 _ acc = acc
sumWeights n weight acc = sumWeights (n - 1) (weight - 1) acc + weight

weighRocks :: Int -> Int -> Int -> [Tile] -> Int
weighRocks finalWeight circleRockCount totalDistance [] = finalWeight + sumWeights circleRockCount totalDistance 0
weighRocks finalWeight circleRockCount totalDistance (EmptyTile : xs) = weighRocks finalWeight circleRockCount (totalDistance + 1) xs
weighRocks finalWeight circleRockCount totalDistance (CircleRock : xs) = weighRocks finalWeight (circleRockCount + 1) (totalDistance + 1) xs
weighRocks finalWeight circleRockCount totalDistance (SquareRock : xs) = weighRocks (finalWeight + sumWeights circleRockCount totalDistance 0) 0 (totalDistance + 1) xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right all -> do
          print $ sum $ map (weighRocks 0 0 0) (rotateField all)
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
