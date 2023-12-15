import Data.List
import Data.Maybe (fromJust)
import Debug.Trace
import GHC.RTS.Flags (DebugFlags (stable))
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

rotateFieldCCW :: [[Tile]] -> [[Tile]]
rotateFieldCCW = reverse . transpose

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

weighField :: [[Tile]] -> Int
weighField field = go 0 1 (reverse field)
  where
    go :: Int -> Int -> [[Tile]] -> Int
    go finalWeight depth [] = finalWeight
    go finalWeight depth (x : xs) = go (finalWeight + depth * length (filter (== CircleRock) x)) (depth + 1) xs

tiltRow :: [Tile] -> [Tile] -> [Tile] -> [Tile] -> [Tile]
tiltRow [] empties circles acc = acc ++ empties ++ circles
tiltRow (EmptyTile : xs) empties circles acc = tiltRow xs (EmptyTile : empties) circles acc
tiltRow (CircleRock : xs) empties circles acc = tiltRow xs empties (CircleRock : circles) acc
tiltRow (SquareRock : xs) empties circles acc = tiltRow xs [] [] (acc ++ empties ++ circles ++ [SquareRock])

tiltField :: [[Tile]] -> [[Tile]]
tiltField = map (\row -> tiltRow row [] [] [])

tiltCycle :: [[Tile]] -> [[Tile]]
tiltCycle field = tiltField $ rotateField $ tiltField $ rotateField $ tiltField $ rotateField $ tiltField $ rotateField field

cycleUntilCycleDetected :: [[Tile]] -> [[[Tile]]] -> ([[Tile]], [[[Tile]]])
cycleUntilCycleDetected field seen
  | field `elem` seen = (field, seen)
  | otherwise = cycleUntilCycleDetected (tiltCycle field) (seen ++ [field])

tp :: IO [[Tile]] -> IO [[Tile]]
tp x = do
  asdf <- x
  let y = tiltCycle asdf
  let weight = weighField y
  putStrLn $ fieldToString y
  print weight
  putStrLn ""
  return y

tpOnce :: IO [[Tile]] -> IO [[Tile]]
tpOnce x = do
  asdf <- x
  let y = tiltField $ rotateField asdf
  putStrLn $ fieldToString y
  print $ weighField y
  putStrLn ""
  putStrLn ""
  putStrLn ""
  return y

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right all -> do
          let (field, seen) = cycleUntilCycleDetected all []
          let cycleStart = fromJust $ elemIndex field seen
          let cycleEnd = length seen
          let cycleLength = cycleEnd - cycleStart
          let cyclesBeforeEnd = (1000000000 - cycleStart) `mod` cycleLength
          let cycledUntilABillion = applyNTimes cyclesBeforeEnd tiltCycle field
          let weightAfterABillionCycles = weighField cycledUntilABillion
          print weightAfterABillionCycles
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"