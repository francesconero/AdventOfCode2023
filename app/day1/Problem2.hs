import Data.Char (isDigit)
import Data.Foldable (foldl')
import System.Environment (getArgs)
import System.IO

keepHeadAndTail :: String -> String
keepHeadAndTail [] = []
keepHeadAndTail [x] = [x, x]
keepHeadAndTail (x : xs) = [x, lastChar xs]
  where
    lastChar [y] = y
    lastChar (_ : ys) = lastChar ys

convertToInteger :: String -> Integer
convertToInteger = read

foldFunction :: (String, String) -> Char -> (String, String)
foldFunction (output, possibleMatch) char = case possibleMatch ++ [char] of
  "1" -> (output ++ "1", "")
  "2" -> (output ++ "2", "")
  "3" -> (output ++ "3", "")
  "4" -> (output ++ "4", "")
  "5" -> (output ++ "5", "")
  "6" -> (output ++ "6", "")
  "7" -> (output ++ "7", "")
  "8" -> (output ++ "8", "")
  "9" -> (output ++ "9", "")
  "one" -> (output ++ "1", "e")
  "two" -> (output ++ "2", "o")
  "three" -> (output ++ "3", "e")
  "four" -> (output ++ "4", "")
  "five" -> (output ++ "5", "e")
  "six" -> (output ++ "6", "")
  "seven" -> (output ++ "7", "n")
  "eight" -> (output ++ "8", "t")
  "nine" -> (output ++ "9", "e")
  "o" -> (output, "o")
  "on" -> (output, "on")
  "t" -> (output, "t")
  "tw" -> (output, "tw")
  "th" -> (output, "th")
  "thr" -> (output, "thr")
  "thre" -> (output, "thre")
  "f" -> (output, "f")
  "fo" -> (output, "fo")
  "fou" -> (output, "fou")
  "fi" -> (output, "fi")
  "fiv" -> (output, "fiv")
  "s" -> (output, "s")
  "si" -> (output, "si")
  "se" -> (output, "se")
  "sev" -> (output, "sev")
  "seve" -> (output, "seve")
  "e" -> (output, "e")
  "ei" -> (output, "ei")
  "eig" -> (output, "eig")
  "eigh" -> (output, "eigh")
  "n" -> (output, "n")
  "ni" -> (output, "ni")
  "nin" -> (output, "nin")
  _ : xs -> foldl' foldFunction (output, "") xs

replaceTextNumbers :: String -> String
replaceTextNumbers input = fst $ foldl' foldFunction ("", "") input

processLine :: String -> Integer
processLine = convertToInteger . keepHeadAndTail . replaceTextNumbers

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      let processedLines = map processLine $ lines contents
      print $ sum processedLines
    _ -> putStrLn "Usage: stack run -- ./input_file"
