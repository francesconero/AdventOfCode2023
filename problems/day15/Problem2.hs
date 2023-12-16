import Control.Monad
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Lens = Lens {lensLabel :: !String, value :: !Int}

instance Show Lens where
  show (Lens label value) = "[" ++ label ++ " " ++ show value ++ "]"

instance Eq Lens where
  (==) (Lens l1 v1) (Lens l2 v2) = l1 == l2

data Operation = Upsert {lens :: !Lens} | Delete {labelToDelete :: !String} deriving (Show)

hashLabel :: String -> Int
hashLabel = foldl' (\acc c -> ((acc + fromEnum c) * 17) `mod` 256) 0

operationParser :: Parsec String (Int, Map.Map Int [Lens]) Operation
operationParser = do
  label <- many (noneOf "=-")
  operationType <- anyChar
  case operationType of
    '=' -> do
      Upsert . Lens label <$> nat
    '-' -> do
      return $ Delete label

updateMap :: Map.Map Int [Lens] -> Operation -> Map.Map Int [Lens]
updateMap m (Upsert lens) = case Map.lookup (hashLabel $ lensLabel lens) m of
  Just lenses ->
    Map.insert
      (hashLabel $ lensLabel lens)
      ( if lens `elem` lenses
          then foldl' (\acc oldLens -> if oldLens == lens then acc ++ [lens] else acc ++ [oldLens]) [] lenses
          else lenses ++ [lens]
      )
      m
  Nothing -> Map.insert (hashLabel $ lensLabel lens) [lens] m
updateMap m (Delete label) = case Map.lookup (hashLabel label) m of
  Just lenses -> Map.insert (hashLabel label) (filter (\lens -> lensLabel lens /= label) lenses) m
  Nothing -> m

tooTired :: Operation -> Parsec String (Int, Map.Map Int [Lens]) (Map.Map Int [Lens])
tooTired op = do
  modifyState (\state -> (fst state, updateMap (snd state) op))
  fmap snd getState

parseAll :: Parsec String (Int, Map.Map Int [Lens]) (Map.Map Int [Lens])
parseAll = sepEndBy (operationParser >>= tooTired) (oneOf ",\n") >>= (\_ -> fmap snd getState)

multiplyByPosition :: [Lens] -> [Int]
multiplyByPosition lenses = zipWith (\lens i -> value lens * i) lenses [1 ..]

multiplyByPositionMap :: Map.Map Int [Lens] -> [Int]
multiplyByPositionMap m = foldl' (\acc (k, lenses) -> acc ++ [(k + 1) * sum (multiplyByPosition lenses)]) [] $ Map.toList m

calculateSum :: Map.Map Int [Lens] -> Int
calculateSum m = sum $ multiplyByPositionMap m

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case runParser (parseAll) (0, Map.empty) "" contents of
        Right all -> print $ calculateSum all
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"