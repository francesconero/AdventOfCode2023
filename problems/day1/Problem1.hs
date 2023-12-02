import System.Environment (getArgs)
import System.IO  
import Data.Char (isDigit)

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

processLine :: String -> Integer
processLine = convertToInteger . keepHeadAndTail . filterNumbers

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            contents <- readFile filePath
            let processedLines = map processLine $ lines contents
            print $ sum processedLines
        _ -> putStrLn "Usage: stack run -- ./input_file"
