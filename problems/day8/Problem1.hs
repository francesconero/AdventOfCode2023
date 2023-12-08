import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Node = Node
  { nodeId :: !String,
    l :: !String,
    r :: !String
  }
  deriving (Show)

parseNode = do
  nodeId <- manyTill letter space
  string "= ("
  l <- manyTill letter (char ',')
  space
  r <- manyTill letter (char ')')
  return (Node nodeId l r)

parseAll = do
  directions <- manyTill letter $ char '\n'
  char '\n'
  nodes <- sepEndBy parseNode $ char '\n'
  return (directions, nodes)

toNodeMap :: [Node] -> Map.Map String Node
toNodeMap nodes = Map.fromList $ map (\x -> (nodeId x, x)) nodes

walkUntilZZZ :: String -> [Node] -> Int
walkUntilZZZ directions nodes = go directions (fromJust (Map.lookup "AAA" nodeMap)) nodeMap 0
  where
    go :: String -> Node -> Map.Map String Node -> Int -> Int
    go (nextDirection : restDirections) (Node "ZZZ" _ _) _ stepsTaken = stepsTaken
    go (nextDirection : restDirections) (Node _ l r) nodeMap stepsTaken =
      case nextDirection of
        'L' -> go (restDirections ++ [nextDirection]) (fromJust (Map.lookup l nodeMap)) nodeMap stepsTaken + 1
        'R' -> go (restDirections ++ [nextDirection]) (fromJust (Map.lookup r nodeMap)) nodeMap stepsTaken + 1
    nodeMap = toNodeMap nodes

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right (directions, nodes) -> print $ walkUntilZZZ directions nodes
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
