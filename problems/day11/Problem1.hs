import Data.Functor
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec

data Galaxy = Galaxy {x :: Int, y :: Int, ident :: Int} deriving (Show)

instance Eq Galaxy where
  (==) a b = ident a == ident b

galaxyParser :: Parsec String Int Galaxy
galaxyParser = do
  pos <- getPosition
  char '#'
  ident <- getState
  modifyState (+ 1)
  return $ Galaxy (sourceLine pos) (sourceColumn pos) ident

parseAll :: Parsec String Int [Galaxy]
parseAll = many (char '.') *> sepEndBy galaxyParser (many (char '.' <|> newline))

groupByX :: [Galaxy] -> [(Int, [Int])]
groupByX = Map.toList . Map.fromListWith (++) . map (\g -> (x g, [ident g]))

groupByY :: [Galaxy] -> [(Int, [Int])]
groupByY = Map.toList . Map.fromListWith (++) . map (\g -> (y g, [ident g]))

expand :: [(Int, [Int])] -> [(Int, [Int])]
expand toExpand =
  let (out, _, _) = foldl' (\(out, lastSeen, e) (k, v) -> (out ++ [(k + k - lastSeen - 1 + e, v)], k, k - lastSeen - 1 + e)) ([], 0, 0) toExpand
   in out

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

invertPairs :: [(Int, [Int])] -> Map.Map Int Int
invertPairs = Map.fromList . concatMap (\(k, v) -> map (\i -> (i, k)) v)

rebuildGalaxies :: [(Int, [Int])] -> [(Int, [Int])] -> [Galaxy]
rebuildGalaxies galaxiesByX galaxiesByY = concatMap (\(x, idents) -> map (\identity -> Galaxy x (galaxyToY Map.! identity) identity) idents) galaxiesByX
  where
    galaxyToY = invertPairs galaxiesByY

distanceBetweenGalaxies :: Galaxy -> Galaxy -> Int
distanceBetweenGalaxies a b = manhattanDistance (x a, y a) (x b, y b)

uniquePairsOfGalaxies :: [Galaxy] -> [(Galaxy, Galaxy)]
uniquePairsOfGalaxies galaxies = [(a, b) | (a : bs) <- tails galaxies, b <- bs]

everyDistance :: [Galaxy] -> [(Galaxy, Galaxy, Int)]
everyDistance galaxies = map (\(a, b) -> (a, b, distanceBetweenGalaxies a b)) $ uniquePairsOfGalaxies galaxies

distance :: (Galaxy, Galaxy, Int) -> Int
distance (_, _, d) = d

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case runP parseAll 0 "" contents of
        Right parsed ->
          let expandedUniverse = rebuildGalaxies (expand $ groupByX parsed) (expand $ groupByY parsed)
           in print $ sum (map distance (everyDistance expandedUniverse))
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
