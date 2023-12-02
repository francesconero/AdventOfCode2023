import Data.Foldable
import Data.Monoid
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

data Color
  = Red
  | Green
  | Blue
  deriving (Eq, Ord, Show)

colorParser =
  (string "red" >> return Red)
    <|> (string "green" >> return Green)
    <|> (string "blue" >> return Blue)
    <?> "either red green or blue"

data ColoredExtraction = ColoredExtraction
  { color :: !Color,
    count :: !Int
  }
  deriving (Show)

coloredExtractionParser = do
  theCount <- int
  space
  theColor <- colorParser
  return (ColoredExtraction theColor theCount)

parseNextColor seen = do
  c <- coloredExtractionParser
  if Set.member (color c) seen
    then fail $ "Duplicate color found: " ++ show (color c)
    else do
      optional (char ',' >> spaces)
      return c

parseColorList seen =
  ( do
      found <- try $ parseNextColor seen
      rest <- parseColorList (Set.insert (color found) seen)
      return (found : rest)
  )
    <|> return []

data Extraction = Extraction
  { redCount :: !Int,
    greenCount :: !Int,
    blueCount :: !Int
  }
  deriving (Show)

instance Semigroup Extraction where
  Extraction x1 y1 z1 <> Extraction x2 y2 z2 = Extraction (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid Extraction where
  mempty = Extraction 0 0 0

parseExtraction = do
  coloredExtractions <- parseColorList Set.empty
  return $
    mconcat $
      map
        ( \(ColoredExtraction color count) -> case color of
            Red -> Extraction count 0 0
            Green -> Extraction 0 count 0
            Blue -> Extraction 0 0 count
        )
        coloredExtractions

parseExtractions = sepBy parseExtraction (char ';' >> spaces)

data Game = Game
  { gameId :: !Int,
    extractions :: ![Extraction]
  }
  deriving (Show)

parseGame = do
  string "Game "
  gameId <- int
  string ": "
  extractions <- parseExtractions
  eof
  return Game {gameId = gameId, extractions = extractions}

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case traverse (parse parseGame "") (lines contents) of
        Right parsedGames ->
          print $
            sum $
              map
                gameId
                ( filter
                    (\(Game id extractions) -> foldl' (\acc (Extraction red green blue) -> acc && red <= 12 && green <= 13 && blue <= 14) True extractions)
                    parsedGames
                )
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
