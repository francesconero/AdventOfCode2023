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

extrapolatePolynomialValue :: Double -> [Double] -> Int
extrapolatePolynomialValue x yVals = round $ sum $ zipWith (*) (calculateCoefficients yVals) (map (x **) [0 ..])

parseAll :: Parser [[Int]]
parseAll = sepEndBy (sepBy1 int (char ' ')) endOfLine

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right seriesList -> print $ sum $ map (\series -> extrapolatePolynomialValue (fromIntegral (length series) + 1) (map fromIntegral series)) seriesList
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
