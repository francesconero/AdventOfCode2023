import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Numeric.LinearAlgebra
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

vandermondeIncremental :: Int -> Matrix Double
vandermondeIncremental n = (n >< n) [fromIntegral (i + 1) ^ j | i <- [0 .. (n - 1)], j <- [0 .. (n - 1)]]

calculateCoefficients :: [Double] -> [Double]
calculateCoefficients yValues =
  let n = length yValues
      vMatrix = vandermondeIncremental n
      yMatrix = (n >< 1) yValues
   in toList $ flatten $ linearSolveLS vMatrix yMatrix

-- because of numerical errors, this doesn't quite work unfortunately
extrapolatePolynomialValue :: Double -> [Double] -> Double
extrapolatePolynomialValue x yVals = sum $ zipWith (*) (calculateCoefficients yVals) (map (x **) [0 ..])

stackedDiffs :: [[Int]] -> [[Int]]
stackedDiffs (allZeroes : rest)
  | all (== 0) allZeroes = rest
stackedDiffs (head : tl) = stackedDiffs (zipWith (-) (tail head) head : (head : tl))

extendRight :: [[Int]] -> [[Int]]
extendRight input = fst $ foldl' (\(out, toAdd) series -> (out ++ [series ++ [last series + toAdd]], last series + toAdd)) ([], 0) input

extendLeft :: [[Int]] -> [[Int]]
extendLeft input = fst $ foldl' (\(out, toSub) series -> (out ++ [(head series - toSub) : series], head series - toSub)) ([], 0) input

extrapolateRight :: [Int] -> Int
extrapolateRight series = last $ last $ extendRight $ stackedDiffs [series]

extrapolateLeft :: [Int] -> Int
extrapolateLeft series = head $ last $ extendLeft $ stackedDiffs [series]

parseAll :: Parser [[Int]]
parseAll = sepEndBy (sepBy1 int (char ' ')) endOfLine

sumTuples :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right seriesList -> do
          print $ foldl' (\acc x -> sumTuples acc x) (0, 0) $ map (\series -> (extrapolatePolynomialValue (fromIntegral (0)) (map fromIntegral series), extrapolateLeft series)) seriesList
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
