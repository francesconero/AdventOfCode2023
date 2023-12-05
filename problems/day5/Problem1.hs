import Data.List
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> sepEndBy nat space

parseMappingRule = do
  s <- nat
  space
  t <- nat
  space
  r <- nat
  return (s, t, r)

transformWithRule :: (Int, Int, Int) -> Int -> Maybe Int
transformWithRule (target, source, range) thing = if thing >= source && thing <= source + range then Just (thing - source + target) else Nothing

transformUntilJust :: Int -> [Int -> Maybe Int] -> Maybe Int
transformUntilJust toTransform =
  foldl'
    ( \acc f -> case acc of
        Just transformed -> Just transformed
        Nothing -> f toTransform
    )
    Nothing

parseMap :: Parser [(Int, Int, Int)]
parseMap = sepEndBy parseMappingRule anyChar

transformThings :: [(Int, Int, Int)] -> [Int] -> [Int]
transformThings rules =
  map
    (\x -> fromMaybe x (transformUntilJust x (map transformWithRule rules)))

parseAll = do
  seeds <- parseSeeds
  manyTill anyChar (lookAhead parseMappingRule)
  maps <- sepBy parseMap (manyTill anyChar (lookAhead parseMappingRule))
  let locations = foldl' (flip transformThings) seeds maps
  return $ minimum locations

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
