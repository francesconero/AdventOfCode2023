import Data.List
import Data.Maybe
import Data.Ord
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

parseSeedRange :: Parser (Int, Int)
parseSeedRange = do
  s <- nat
  space
  e <- nat
  return (s, e)

parseSeedRanges :: Parser [(Int, Int)]
parseSeedRanges = string "seeds: " *> sepEndBy parseSeedRange space

parseMappingRule = do
  s <- nat
  space
  t <- nat
  space
  r <- nat
  return (s, t, r)

splitRangeWithRule :: (Int, Int, Int) -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
splitRangeWithRule (target, source, rangeTransform) (start, range)
  | start + range <= source = ([], [(start, range)])
  | start < source && start + range > source + rangeTransform = ([(target, rangeTransform)], [(start, source - start), (source + rangeTransform + 1, start + range - source - rangeTransform)])
  | start < source = ([(target, start + range - source)], [(start, source - start)])
  | start + range <= source + rangeTransform = ([(start - source + target, range)], [])
  | start < source + rangeTransform && start + range > source + rangeTransform = ([(start - source + target, source + rangeTransform - start)], [(source + rangeTransform, start + range - (source + rangeTransform))])
  | otherwise = ([], [(start, range)])

splitRangeWithRules :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
splitRangeWithRules rules toTransform =
  let (transformed, unchanged) =
        foldl'
          ( \(transformed, unchanged) rule -> case unzip $ map (splitRangeWithRule rule) unchanged of
              (transformedList, unchangedList) -> (concat transformedList ++ transformed, concat unchangedList)
          )
          ([], [toTransform])
          rules
   in transformed ++ unchanged

parseMap :: Parser [(Int, Int, Int)]
parseMap = sepEndBy parseMappingRule anyChar

parseAll = do
  seedRanges <- parseSeedRanges
  manyTill anyChar (lookAhead parseMappingRule)
  maps <- sepBy parseMap (manyTill anyChar (lookAhead parseMappingRule))
  let locations = foldl' (\acc x -> concatMap (splitRangeWithRules x) acc) seedRanges maps
  return $ fst $ minimumBy (comparing fst) locations

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right parsed -> print parsed
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
