import Data.Functor
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec

charParser :: Parsec String Int Char
charParser = do
  optional newline
  c <- noneOf ","
  optional newline
  current <- getState
  modifyState (+ fromEnum c)
  modifyState (* 17)
  modifyState (`mod` 256)
  return c

initializationSequenceParser :: Parsec String Int String
initializationSequenceParser = do
  setState 0
  many charParser

parseAll :: Parsec String Int [Int]
parseAll = do
  sepEndBy (initializationSequenceParser >> getState) (oneOf ",\n")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case runParser (parseAll) 0 "" contents of
        Right all -> print $ sum all
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"