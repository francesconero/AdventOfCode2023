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

levehsteinDistance :: (Eq a) => [a] -> [a] -> Int
levehsteinDistance xs ys = sum [if x /= y then 1 else 0 | (x, y) <- zip xs ys]

findSymmetryLine :: Int -> Int -> Int -> [[Tile]] -> Int
findSymmetryLine offset span smudges field
  | offset + 1 >= length field = 0
  | offset + span >= length field || (offset - span + 1) < 0 = if smudges == 1 then offset + 1 else findSymmetryLine (offset + 1) 1 0 field
  | (field !! (offset - span + 1)) == (field !! (offset + span)) = findSymmetryLine offset (span + 1) smudges field
  | levehsteinDistance (field !! (offset - span + 1)) (field !! (offset + span)) == 1 && smudges == 0 = findSymmetryLine offset (span + 1) 1 field
  | otherwise = findSymmetryLine (offset + 1) 1 0 field

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
        Right all ->
          print $
            sum $
              map
                ( \field ->
                    let simmetry1 = findSymmetryLine 0 1 0 (rotateField field)
                        simmetry2 = findSymmetryLine 0 1 0 field
                     in simmetry1 + (100 * simmetry2)
                )
                all
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
