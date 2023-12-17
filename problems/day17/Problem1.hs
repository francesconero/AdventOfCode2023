import Algorithm.Search
import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Data.Map.Internal.Debug (node)
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec hiding (Empty)
import Text.Parsec.String

data Node = Node {x :: !Int, y :: !Int, heat :: !Int} deriving (Eq, Ord)

instance Show Node where
  show (Node x y heat) = "(" ++ show (x, y, heat) ++ ")"

data VisitedNode = VisitedNode {node :: !Node, lastVisited :: !(Maybe VisitedNode), straightCount :: !Int} deriving (Eq, Ord)

instance Show VisitedNode where
  show (VisitedNode (Node x y heat) lastVisited straightcount) = "[" ++ show (x, y, heat, lastVisited, straightcount) ++ "]"

parseNode :: Parser Node
parseNode = do
  pos <- getPosition
  let x = sourceColumn pos
  let y = sourceLine pos
  cost <- digit
  optional newline
  return $ Node x y (read [cost])

nodesToMap :: [Node] -> Map.Map (Int, Int) Node
nodesToMap nodes = Map.fromList $ map (\node -> ((x node, y node), Node (x node) (y node) (heat node))) nodes

parseAll = many parseNode

taxiCabCross :: Map.Map (Int, Int) Node -> Node -> [Node]
taxiCabCross m (Node x y _) = mapMaybe (`Map.lookup` m) [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

taxiCabStraight :: Map.Map (Int, Int) Node -> Maybe (Node, Node) -> Int -> Maybe Node
taxiCabStraight _ Nothing _ = Nothing
taxiCabStraight m (Just (Node x1 y1 heat, Node x2 y2 heat2)) straightCount = Map.lookup (x2 + (x2 - x1), y2 + (y2 - y1)) m

neighbors :: Map.Map (Int, Int) Node -> VisitedNode -> [VisitedNode]
neighbors m (VisitedNode n previousNode straightCount) =
  let allAround = taxiCabCross m n
      straight = taxiCabStraight m (fmap (\(VisitedNode x _ straightCount) -> (x, n)) previousNode) straightCount
      withoutStraight = filter (`notElem` straight) allAround
      withoutPrevious = filter (\x -> x `notElem` fmap Main.node previousNode) withoutStraight
      newPreviousNode neighbor = VisitedNode n Nothing 0
      neighbors = map (\x -> VisitedNode x (Just (newPreviousNode x)) 1) withoutPrevious ++ if straightCount < 3 then map (\x -> VisitedNode x (Just (VisitedNode n Nothing straightCount)) (straightCount + 1)) (maybeToList straight) else []
   in neighbors

calculateCost :: VisitedNode -> VisitedNode -> Int
calculateCost _ (VisitedNode (Node _ _ heat) _ _) = heat

taxiCabDistance :: (Int, Int) -> VisitedNode -> Int
taxiCabDistance (x2, y2) (VisitedNode (Node x1 y1 _) _ _) = abs (x1 - x2) + abs (y1 - y2)

findPath :: Map.Map (Int, Int) Node -> (Int, Int) -> (Int, Int) -> Maybe (Int, [VisitedNode])
findPath m (startX, startY) (endX, endY) =
  let next = neighbors m
      cost = calculateCost
      remaining = taxiCabDistance (endX, endY)
      found (VisitedNode (Node x y cost) _ _) = (x, y) == (endX, endY)
      start = VisitedNode (Node startX startY 0) Nothing 0
   in aStar next cost remaining found start

lowerRightcorner :: Map.Map (Int, Int) Node -> (Int, Int)
lowerRightcorner m = maximumBy (\(x1, y1) (x2, y2) -> if x1 > x2 then GT else if x2 > x1 then LT else if y1 > y2 then GT else if y2 > y1 then LT else EQ) (Map.keys m)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right all ->
          let map = nodesToMap all
              start = (1, 1)
              end = lowerRightcorner map
           in traceShow
                end
                ( case findPath map start end of
                    Just (cost, _) -> print cost
                    Nothing -> putStrLn "No path found"
                )
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
