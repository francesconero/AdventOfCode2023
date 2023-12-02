import System.Environment (getArgs)
import System.IO  
import Data.Char (isDigit)
import qualified Data.Text as T

filterNumbers :: String -> String
filterNumbers = filter isDigit

keepHeadAndTail :: String -> String
keepHeadAndTail [] = []
keepHeadAndTail [x] = [x, x]
keepHeadAndTail (x:xs) = [x, lastChar xs]
  where
    lastChar [y] = y
    lastChar (_:ys) = lastChar ys

convertToInteger :: String -> Integer
convertToInteger = read

foldFunction :: (String, String) -> Char -> (String, String)
foldFunction (output, possibleMatch) char = case possibleMatch ++ [char] of
    "one" -> (output ++ "1", "e")
    "two" -> (output ++ "2", "o")
    "three" -> (output ++ "3", "e")
    "four" -> (output ++ "4", "")
    "five" -> (output ++ "5", "e")
    "six" -> (output ++ "6", "")
    "seven" -> (output ++ "7", "n")
    "eight" -> (output ++ "8", "t")
    "nine" -> (output ++ "9", "e")
    "o" -> (output, possibleMatch ++ [char])
    "on" -> (output, possibleMatch ++ [char])
    "t" -> (output, possibleMatch ++ [char])
    "tw" -> (output, possibleMatch ++ [char])
    "th" -> (output, possibleMatch ++ [char])
    "thr" -> (output, possibleMatch ++ [char])
    "thre" -> (output, possibleMatch ++ [char])
    "f" -> (output, possibleMatch ++ [char])
    "fo" -> (output, possibleMatch ++ [char])
    "fou" -> (output, possibleMatch ++ [char])
    "fi" -> (output, possibleMatch ++ [char])
    "fiv" -> (output, possibleMatch ++ [char])
    "s" -> (output, possibleMatch ++ [char])
    "si" -> (output, possibleMatch ++ [char])
    "se" -> (output, possibleMatch ++ [char])
    "sev" -> (output, possibleMatch ++ [char])
    "seve" -> (output, possibleMatch ++ [char])
    "e" -> (output, possibleMatch ++ [char])
    "ei" -> (output, possibleMatch ++ [char])
    "eig" -> (output, possibleMatch ++ [char])
    "eigh" -> (output, possibleMatch ++ [char])
    "n" -> (output, possibleMatch ++ [char])
    "ni" -> (output, possibleMatch ++ [char])
    "nin" -> (output, possibleMatch ++ [char])
    x:xs -> foldl foldFunction (output ++ [x], "") xs

replaceTextNumbers :: String -> String
replaceTextNumbers input = fst $ foldl foldFunction ("", "") input

processLine :: String -> Integer
processLine = convertToInteger . keepHeadAndTail . filterNumbers . replaceTextNumbers


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            contents <- readFile filePath
            let processedLines = map processLine $ lines contents
            print $ sum processedLines
        _ -> putStrLn "Usage: stack run -- ./input_file"
