import Data.List
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Tile = Ash | Rock deriving (Show, Eq)

parseTile :: Parser Tile
parseTile = do
  c <- oneOf ".#"
  return $ case c of
    '.' -> Ash
    '#' -> Rock

parseField :: Parser [[Tile]]
parseField = sepEndBy (many1 parseTile) endOfLine

parseAll :: Parser [[[Tile]]]
parseAll = sepBy parseField endOfLine

rotateField :: [[Tile]] -> [[Tile]]
rotateField = transpose

findSymmetryLine :: Int -> Int -> [[Tile]] -> Int
findSymmetryLine offset span field
  | offset + 1 >= length field = 0
  | offset + span >= length field || (offset - span + 1) < 0 = offset + 1
  | (field !! (offset - span + 1)) == (field !! (offset + span)) = findSymmetryLine offset (span + 1) field
  | otherwise = findSymmetryLine (offset + 1) 1 field

fieldToString :: [[Tile]] -> String
fieldToString field =
  intercalate "\n" $
    map
      ( map
          ( \tile -> case tile of
              Ash -> '.'
              Rock -> '#'
          )
      )
      field

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right all -> print $ sum $ map (\field -> findSymmetryLine 0 1 (rotateField field) + (100 * findSymmetryLine 0 1 field)) all
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
