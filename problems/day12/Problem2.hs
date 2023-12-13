import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

type Cache = Map.Map ([Piece], [Int]) Int

data Piece = W | B | U deriving (Show, Eq, Ord)

parsePiece :: Parser Piece
parsePiece =
  do
    char '.'
    return W
    <|> do
      char '#'
      return B
    <|> do
      char '?'
      return U

parsePieces :: Parser [Piece]
parsePieces = many1 parsePiece

parseBrokenGroups :: Parser [Int]
parseBrokenGroups = sepBy nat (char ',')

parsePiecesAndBrokenGroups :: Parser ([Piece], [Int])
parsePiecesAndBrokenGroups =
  do
    pieces <- parsePieces
    spaces
    brokenGroups <- parseBrokenGroups
    return (pieces, brokenGroups)

parseAll :: Parser [([Piece], [Int])]
parseAll = sepEndBy parsePiecesAndBrokenGroups (char '\n')

brokenGroupCounts :: [Piece] -> [Int]
brokenGroupCounts [] = []
brokenGroupCounts (x : xs) =
  let (firstGroup, rest) = span (== x) xs
   in if x == B then length (x : firstGroup) : brokenGroupCounts rest else brokenGroupCounts rest

-- for every unknown, generate a list of all possible combinations of working/broken, but only for combinations in which the number of broken and working pieces match a given
materializeUnknowns2 :: [Piece] -> Int -> Int -> [[Piece]]
materializeUnknowns2 [] _ _ = [[]]
materializeUnknowns2 (x : xs) workingCount brokenCount = case x of
  U -> workingPieces ++ brokenPieces
  _ -> map (x :) rest
  where
    rest = materializeUnknowns2 xs workingCount brokenCount

    workingPieces
      | workingCount > 0 = map (W :) $ materializeUnknowns2 xs (workingCount - 1) brokenCount
      | otherwise = []

    brokenPieces
      | brokenCount > 0 = map (B :) $ materializeUnknowns2 xs workingCount (brokenCount - 1)
      | otherwise = []

countStuff :: [Piece] -> [Int] -> Int
countStuff [] [] = 1
countStuff [] _ = 0
countStuff (first : remainingPieces) groups = case first of
  W -> countStuff remainingPieces groups
  B -> case groups of
    [] -> 0
    (group : groups) ->
      ( if length remainingPieces < (group - 1)
          then 0
          else case splitAt (group - 1) remainingPieces of
            (mustBeBroken, mustBeWorkingAndRemainingOrEmpty) ->
              if W `notElem` mustBeBroken
                then
                  if null mustBeWorkingAndRemainingOrEmpty
                    then if null groups then 1 else 0
                    else case mustBeWorkingAndRemainingOrEmpty of
                      (W : remaining) -> countStuff remaining groups
                      (U : remaining) -> countStuff remaining groups
                      _ -> 0
                else 0
      )
  U -> countStuff (W : remainingPieces) groups + countStuff (B : remainingPieces) groups

countStuffMemo :: [Piece] -> [Int] -> Int
countStuffMemo pieces groups = fst $ countStuff' pieces groups Map.empty

countStuff' :: [Piece] -> [Int] -> Cache -> (Int, Cache)
countStuff' [] [] cache = (1, cache)
countStuff' [] _ cache = (0, cache)
countStuff' (first : remainingPieces) groups cache =
  case Map.lookup (first : remainingPieces, groups) cache of
    Just result -> (result, cache)
    Nothing ->
      let (result, newCache) = case first of
            W -> countStuff' remainingPieces groups cache
            B -> case groups of
              [] -> (0, cache)
              (group : groups) ->
                ( if length remainingPieces < (group - 1)
                    then (0, cache)
                    else case splitAt (group - 1) remainingPieces of
                      (mustBeBroken, mustBeWorkingAndRemainingOrEmpty) ->
                        if W `notElem` mustBeBroken
                          then
                            if null mustBeWorkingAndRemainingOrEmpty
                              then if null groups then (1, cache) else (0, cache)
                              else case mustBeWorkingAndRemainingOrEmpty of
                                (W : remaining) -> countStuff' remaining groups cache
                                (U : remaining) -> countStuff' remaining groups cache
                                _ -> (0, cache)
                          else (0, cache)
                )
            U ->
              let (resW, cacheW) = countStuff' (W : remainingPieces) groups cache
                  (resB, cacheB) = countStuff' (B : remainingPieces) groups cacheW
               in (resW + resB, cacheB)
       in (result, Map.insert (first : remainingPieces, groups) result newCache)

unfoldPieces :: [Piece] -> [Piece]
unfoldPieces pieces = concat [x ++ [U] | x <- init xs] ++ last xs
  where
    xs = replicate 5 pieces

unfoldGroupCount :: [Int] -> [Int]
unfoldGroupCount groupCount = concat $ replicate 5 groupCount

countUnknowns :: [Piece] -> Int
countUnknowns [] = 0
countUnknowns (x : xs) = if x == U then 1 + countUnknowns xs else countUnknowns xs

countBroken :: [Piece] -> Int
countBroken [] = 0
countBroken (x : xs) = if x == B then 1 + countBroken xs else countBroken xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right all -> do
          print $
            sum $
              map
                ( \(pieces, brokenGroups) ->
                    let unfoldedPieces = unfoldPieces pieces
                        unfoldedGroups = unfoldGroupCount brokenGroups
                        workingCount = countUnknowns unfoldedPieces - sum unfoldedGroups + countBroken unfoldedPieces
                        brokenCount = sum unfoldedGroups - countBroken unfoldedPieces
                        out = countStuffMemo unfoldedPieces unfoldedGroups
                     in out
                )
                all
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"