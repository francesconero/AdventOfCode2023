import Data.Functor
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data TileType = Empty | Start | PipeNS | PipeEW | PipeNE | PipeNW | PipeSE | PipeSW deriving (Show, Eq)

data Tile = Tile
  { tileType :: !TileType,
    position :: !(Int, Int)
  }
  deriving (Show, Eq)

type TileMap = Map.Map (Int, Int) Tile

parseTile :: Parser Tile
parseTile = do
  pos <- getPosition
  tileType <-
    anyChar
      >>= ( \c ->
              case c of
                '.' -> return Main.Empty
                '|' -> return PipeNS
                '-' -> return PipeEW
                'S' -> return Start
                'L' -> return PipeNE
                'J' -> return PipeNW
                'F' -> return PipeSE
                '7' -> return PipeSW
                other -> fail $ "Invalid tile type at " ++ show pos ++ ": " ++ show other
          )
  return $ Tile tileType (sourceColumn pos, sourceLine pos)

parseAll :: Parser [Tile]
parseAll = many $ try (optional space *> parseTile)

toTileMap :: [Tile] -> TileMap
toTileMap = Map.fromList . map (\tile -> (position tile, tile))

findStart :: Map.Map (Int, Int) Tile -> Tile
findStart tiles = fromJust $ find (\tile -> tileType tile == Start) $ Map.elems tiles

findTilePorts :: TileMap -> Tile -> [Tile]
findTilePorts tileMap tile =
  let (tilePosx, tilePosY) = position tile
      portPositions = case tileType tile of
        PipeNS -> [(tilePosx, tilePosY - 1), (tilePosx, tilePosY + 1)]
        PipeEW -> [(tilePosx - 1, tilePosY), (tilePosx + 1, tilePosY)]
        PipeNE -> [(tilePosx + 1, tilePosY), (tilePosx, tilePosY - 1)]
        PipeNW -> [(tilePosx, tilePosY - 1), (tilePosx - 1, tilePosY)]
        PipeSE -> [(tilePosx + 1, tilePosY), (tilePosx, tilePosY + 1)]
        PipeSW -> [(tilePosx - 1, tilePosY), (tilePosx, tilePosY + 1)]
        Start -> [(tilePosx + 1, tilePosY), (tilePosx, tilePosY + 1), (tilePosx - 1, tilePosY), (tilePosx, tilePosY - 1)]
        _ -> []
   in mapMaybe (`Map.lookup` tileMap) portPositions

findFlowingTo :: TileMap -> Tile -> [Tile]
findFlowingTo tileMap sourceTile =
  let sourcePorts = findTilePorts tileMap sourceTile
   in filter (any (\targetPort -> position targetPort == position sourceTile) . findTilePorts tileMap) sourcePorts

step :: TileMap -> (Int, Tile, Tile) -> [(Int, Tile, Tile)]
step tileMap (stepsTaken, previousTile, currentTile) =
  let flowingTo = findFlowingTo tileMap currentTile
   in filter (\(_, _, tile) -> position tile /= position previousTile) $ map (\nextTile -> (stepsTaken + 1, currentTile, nextTile)) flowingTo

walkUntil :: TileMap -> (Tile -> Bool) -> Tile -> [(Int, Tile, Tile)]
walkUntil tileMap predicate startTile =
  let start = (0, startTile, startTile)
      walkUntil' (stepsTaken, previousTile, currentTile) =
        if predicate currentTile
          then [(stepsTaken, previousTile, currentTile)]
          else step tileMap (stepsTaken, previousTile, currentTile) >>= walkUntil'
   in walkUntil' start

isStart :: Tile -> Bool
isStart tile = tileType tile == Start

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right tiles ->
          let tileMap = toTileMap tiles
              start = findStart tileMap
              firstStep = head $ findFlowingTo tileMap start
              destinations = walkUntil tileMap isStart firstStep
              (stepsTaken, _, _) = maximumBy (\(steps1, _, _) (steps2, _, _) -> compare steps1 steps2) destinations
              halfway = ceiling $ fromIntegral stepsTaken / 2
           in print halfway
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
