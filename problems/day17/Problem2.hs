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
  show (Node x y heat) = show (x, y, heat)

data Direction = N | E | S | W deriving (Eq, Ord, Show)

data VisitedNode = VisitedNode {node :: !Node, direction :: !(Maybe Direction), straightCount :: !Int} deriving (Eq, Ord)

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

taxiCabCross :: Map.Map (Int, Int) Node -> Int -> Node -> [Node]
taxiCabCross m l (Node x y _) = mapMaybe (`Map.lookup` m) [(x - l, y), (x, y - l), (x + l, y), (x, y + l)]

taxiCabStraight :: Map.Map (Int, Int) Node -> Maybe (Node, Node) -> Int -> Maybe Node
taxiCabStraight _ Nothing _ = Nothing
taxiCabStraight m (Just (Node x1 y1 heat, Node x2 y2 heat2)) l =
  let xDistance = x2 - x1
      yDistance = y2 - y1
   in if xDistance > 0
        then Map.lookup (x2 + l, y2) m
        else
          if yDistance > 0
            then Map.lookup (x2, y2 + l) m
            else Nothing

removeSameDirection :: Maybe (Node, Node) -> [Node] -> [Node]
removeSameDirection direction original = out
  where
    out = maybe original remove direction
    remove (Node x1 y1 _, Node x2 y2 _)
      | x2 > x1 = filter (\(Node x y _) -> x <= x2) original
      | x1 > x2 = filter (\(Node x y _) -> x >= x2) original
      | y2 > y1 = filter (\(Node x y _) -> y <= y2) original
      | y1 > y2 = filter (\(Node x y _) -> y >= y2) original
      | otherwise = original

taxiCabPath :: Map.Map (Int, Int) Node -> Node -> Node -> [Node]
taxiCabPath m (Node x1 y1 _) (Node x2 y2 _) = catMaybes [if (x, y) /= (x1, y1) then Map.lookup (x, y) m else Nothing | x <- if x1 <= x2 then [x1 .. x2] else [x2 .. x1], y <- if y1 <= y2 then [y1 .. y2] else [y2 .. y1]]

pathCost :: [Node] -> Int
pathCost = sum . fmap heat

goInDirection :: Map.Map (Int, Int) Node -> Int -> Node -> Direction -> Maybe Node
goInDirection m l (Node x y _) N = Map.lookup (x, y - l) m
goInDirection m l (Node x y _) E = Map.lookup (x + l, y) m
goInDirection m l (Node x y _) S = Map.lookup (x, y + l) m
goInDirection m l (Node x y _) W = Map.lookup (x - l, y) m

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft E = N
turnLeft S = E
turnLeft W = S

neighbors :: Map.Map (Int, Int) Node -> VisitedNode -> [VisitedNode]
neighbors m (VisitedNode n maybeDir straightCount) =
  let out = case maybeDir of
        Nothing -> mapMaybe (\direction -> fmap (\newNode -> VisitedNode newNode (Just direction) 4) (goInDirection m 4 n direction)) [N, E, S, W]
        Just dir -> (if straightCount < 10 then maybeToList (fmap (\newNode -> VisitedNode newNode (Just dir) (straightCount + 1)) (goInDirection m 1 n dir)) else []) ++ maybeToList (fmap (\newNode -> VisitedNode newNode (Just (turnRight dir)) 4) (goInDirection m 4 n (turnRight dir))) ++ maybeToList (fmap (\newNode -> VisitedNode newNode (Just (turnLeft dir)) 4) (goInDirection m 4 n (turnLeft dir)))
   in out

calculateCost :: Map.Map (Int, Int) Node -> VisitedNode -> VisitedNode -> Int
calculateCost m (VisitedNode node1 _ _) (VisitedNode node2 _ _) =
  let path = taxiCabPath m node1 node2
      out = sum . fmap heat $ path
   in out

taxiCabDistance :: (Int, Int) -> VisitedNode -> Int
taxiCabDistance (x2, y2) (VisitedNode (Node x1 y1 _) _ _) = abs (x1 - x2) + abs (y1 - y2)

findPath :: Map.Map (Int, Int) Node -> (Int, Int) -> (Int, Int) -> Maybe (Int, [VisitedNode])
findPath m (startX, startY) (endX, endY) =
  let next = neighbors m
      cost = calculateCost m
      remaining = taxiCabDistance (endX, endY)
      found (VisitedNode (Node x y cost) _ _) = (x, y) == (endX, endY)
      start = VisitedNode (Map.findWithDefault (Node startX startY 0) (startX, startY) m) Nothing 0
   in aStar next cost remaining found start

lowerRightcorner :: Map.Map (Int, Int) Node -> (Int, Int)
lowerRightcorner m = maximumBy (\(x1, y1) (x2, y2) -> if x1 > x2 then GT else if x2 > x1 then LT else if y1 > y2 then GT else if y2 > y1 then LT else EQ) (Map.keys m)

asRows :: Map.Map (Int, Int) Node -> [[Node]]
asRows inputMap =
  let rows = Map.toList inputMap
      maxRow = maximum (map (fst . fst) rows)
      maxCol = maximum (map (snd . fst) rows)
   in filter (not . null) [catMaybes [Map.lookup (row, col) inputMap | row <- [1 .. maxRow]] | col <- [1 .. maxCol]]

printNode :: [Node] -> Node -> String
printNode pathNodes (Node x y heat) = (if Node x y heat `elem` pathNodes then "*" else show heat)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right all ->
          let m = nodesToMap all
              start = (1, 1)
              end = lowerRightcorner m
           in case findPath m start end of
                Just (cost, path) -> do
                  mapM_ putStrLn (map (\row -> concatMap (printNode (fmap Main.node path)) row) (asRows m))
                  print (cost, fmap Main.node path)
                Nothing -> putStrLn "No path found"
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
