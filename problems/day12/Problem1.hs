import Data.List
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Piece = Working | Broken | Unknown deriving (Show, Eq)

parsePiece :: Parser Piece
parsePiece =
  do
    char '.'
    return Working
    <|> do
      char '#'
      return Broken
    <|> do
      char '?'
      return Unknown

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
   in if x == Broken then length (x : firstGroup) : brokenGroupCounts rest else brokenGroupCounts rest

-- for every unknown, generate a list of all possible combinations of working/broken
materializeUnknowns :: [Piece] -> [[Piece]]
materializeUnknowns [] = [[]]
materializeUnknowns (x : xs) =
  let rest = materializeUnknowns xs
   in if x == Unknown then map (Working :) rest ++ map (Broken :) rest else map (x :) rest

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right all -> do
          print $ sum $ map (\(pieces, brokenGroups) -> length $ filter (\materialized -> brokenGroups == brokenGroupCounts materialized) (materializeUnknowns pieces)) all
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
