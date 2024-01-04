import Data.Foldable as Foldable
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Sequence as Sequence
import Debug.Trace
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Text.Printf

data TileType = Rock | Garden deriving (Show, Eq, Ord)

data Tile = Tile {tileType :: TileType, x :: Int, y :: Int, starting :: Bool} deriving (Show, Eq, Ord)

parseTileType :: Parser (TileType, Bool)
parseTileType =
  do
    char '#'
    return (Rock, False)
    <|> do
      char '.'
      return (Garden, False)
    <|> do
      char 'S'
      return (Garden, True)

parseTile :: Parser Tile
parseTile = do
  pos <- getPosition
  (tileType, starting) <- parseTileType
  return $ Tile tileType (sourceColumn pos) (sourceLine pos) starting

parseAll = many1 (parseTile <* optional newline) >>= return . Map.fromList . List.map (\(Tile tileType x y starting) -> ((x, y), Tile tileType x y starting))

findStartingTile :: Map k Tile -> Maybe Tile
findStartingTile tiles = List.find (\(Tile _ _ _ starting) -> starting) (Map.elems tiles)

step :: Map (Int, Int) Tile -> Tile -> [Tile]
step tiles (Tile _ sX sY _) =
  let neighbors = Maybe.mapMaybe (`Map.lookup` tiles) [(sX - 1, sY), (sX + 1, sY), (sX, sY - 1), (sX, sY + 1)]
   in List.filter (\(Tile tileType _ _ _) -> tileType == Garden) neighbors

stepNTimes :: Map (Int, Int) Tile -> Tile -> Int -> [Tile]
stepNTimes tiles tile n = fst $ go tiles tile n Map.empty
  where
    go :: Map (Int, Int) Tile -> Tile -> Int -> Map (Tile, Int) [Tile] -> ([Tile], Map (Tile, Int) [Tile])
    go tiles tile 0 cache = ([], cache)
    go tiles tile 1 cache =
      let lookupResult = Map.lookup (tile, 1) cache
       in case lookupResult of
            Just tilesFound -> (tilesFound, cache)
            Nothing ->
              let tiles' = step tiles tile
                  newCache = Map.insert (tile, 1) tiles' cache
               in (tiles', newCache)
    go tiles tile n cache =
      let lookupResult = Map.lookup (tile, n) cache
       in case lookupResult of
            Just tilesFound -> (tilesFound, cache)
            Nothing ->
              let (nextTiles, newCache) = go tiles tile 1 cache
                  (a, newCache') = List.foldl' (\(out, c) tile -> let (nOut, nCache) = go tiles tile (n - 1) c in (nOut ++ out, nCache)) ([], newCache) nextTiles
                  b = List.nub a
                  newCache'' = Map.insert (tile, n) b newCache'
               in (b, newCache'')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right parsed -> do
          let startingTile = Maybe.fromJust $ findStartingTile parsed
          print $ List.length $ stepNTimes parsed startingTile 64
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"