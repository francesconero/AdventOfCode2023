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
  deriving (Eq)

instance Show Tile where
  show (Tile tileType position) = show tileType ++ " " ++ show position

data Turn = LeftTurn | RightTurn | Straight deriving (Show, Eq)

data VisitedTile = VisitedTile
  { tile :: !Tile,
    turn :: !Turn,
    leftTile :: ![Tile],
    rightTile :: ![Tile]
  }
  deriving (Eq)

instance Show VisitedTile where
  show (VisitedTile tile turn leftTile rightTile) = "V{" ++ show tile ++ " " ++ show turn ++ " " ++ show leftTile ++ " " ++ show rightTile ++ "}"

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

parseTileRow :: Parser [Tile]
parseTileRow = many $ try parseTile

parseAll :: Parser [[Tile]]
parseAll = sepEndBy parseTileRow space

toTileMap :: [Tile] -> TileMap
toTileMap = Map.fromList . map (\tile -> (position tile, tile))

findStart :: Map.Map (Int, Int) Tile -> Tile
findStart tiles = fromJust $ find (\tile -> tileType tile == Start) $ Map.elems tiles

tileByPosition :: TileMap -> (Int, Int) -> Maybe Tile
tileByPosition tileMap position = Map.lookup position tileMap

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
   in mapMaybe (tileByPosition tileMap) portPositions

findFlowingTo :: TileMap -> Tile -> [Tile]
findFlowingTo tileMap sourceTile =
  let sourcePorts = findTilePorts tileMap sourceTile
   in filter (any (\targetPort -> position targetPort == position sourceTile) . findTilePorts tileMap) sourcePorts

data Direction = North | East | South | West | Center deriving (Show, Eq)

inferType :: TileMap -> Tile -> TileType
inferType tileMap currentTile =
  let [tile1, tile2] = findFlowingTo tileMap currentTile
      (currentTilePosX, currentTilePosY) = position currentTile
      (tile1PosX, tile1PosY) = position tile1
      (tile2PosX, tile2PosY) = position tile2
      ew
        | (min tile1PosX tile2PosX < currentTilePosX) = West
        | (max tile1PosX tile2PosX > currentTilePosX) = East
        | otherwise = Center
      ns
        | (min tile1PosY tile2PosY < currentTilePosY) = North
        | (max tile1PosY tile2PosY > currentTilePosY) = South
        | otherwise = Center
      out = case (ew, ns) of
        (West, North) -> PipeNW
        (West, South) -> PipeSW
        (East, North) -> PipeNE
        (East, South) -> PipeSE
        (Center, North) -> PipeNS
        (Center, South) -> PipeNS
        (West, Center) -> PipeEW
        (East, Center) -> PipeEW
   in out

asVisitedTile :: TileMap -> Tile -> Tile -> VisitedTile
asVisitedTile tileMap nextTile currentTile =
  let (currentTilePosX, currentTilePosY) = position currentTile
      (nextTilePosX, nextTilePosY) = position nextTile
      lookupTile = tileByPosition tileMap
   in ( case tileType currentTile of
          PipeNS ->
            ( if nextTilePosY > currentTilePosY
                then VisitedTile currentTile Straight (catMaybes [lookupTile (currentTilePosX + 1, currentTilePosY)]) (catMaybes [lookupTile (currentTilePosX - 1, currentTilePosY)])
                else VisitedTile currentTile Straight (catMaybes [lookupTile (currentTilePosX - 1, currentTilePosY)]) (catMaybes [lookupTile (currentTilePosX + 1, currentTilePosY)])
            )
          PipeEW ->
            ( if nextTilePosX > currentTilePosX
                then VisitedTile currentTile Straight (catMaybes [lookupTile (currentTilePosX, currentTilePosY - 1)]) (catMaybes [lookupTile (currentTilePosX, currentTilePosY + 1)])
                else VisitedTile currentTile Straight (catMaybes [lookupTile (currentTilePosX, currentTilePosY + 1)]) (catMaybes [lookupTile (currentTilePosX, currentTilePosY - 1)])
            )
          PipeNW ->
            ( if currentTilePosY > nextTilePosY
                then VisitedTile currentTile LeftTurn [] (catMaybes [lookupTile (currentTilePosX + 1, currentTilePosY), lookupTile (currentTilePosX + 1, currentTilePosY + 1), lookupTile (currentTilePosX, currentTilePosY + 1)])
                else VisitedTile currentTile RightTurn (catMaybes [lookupTile (currentTilePosX + 1, currentTilePosY), lookupTile (currentTilePosX + 1, currentTilePosY + 1), lookupTile (currentTilePosX, currentTilePosY + 1)]) []
            )
          PipeNE ->
            ( if currentTilePosY > nextTilePosY
                then VisitedTile currentTile RightTurn (catMaybes [lookupTile (currentTilePosX - 1, currentTilePosY), lookupTile (currentTilePosX - 1, currentTilePosY + 1), lookupTile (currentTilePosX, currentTilePosY + 1)]) []
                else VisitedTile currentTile LeftTurn [] (catMaybes [lookupTile (currentTilePosX - 1, currentTilePosY), lookupTile (currentTilePosX - 1, currentTilePosY + 1), lookupTile (currentTilePosX, currentTilePosY + 1)])
            )
          PipeSE ->
            ( if currentTilePosY < nextTilePosY
                then VisitedTile currentTile LeftTurn [] (catMaybes [lookupTile (currentTilePosX - 1, currentTilePosY), lookupTile (currentTilePosX - 1, currentTilePosY - 1), lookupTile (currentTilePosX, currentTilePosY - 1)])
                else VisitedTile currentTile RightTurn (catMaybes [lookupTile (currentTilePosX - 1, currentTilePosY), lookupTile (currentTilePosX - 1, currentTilePosY - 1), lookupTile (currentTilePosX, currentTilePosY - 1)]) []
            )
          PipeSW ->
            ( if currentTilePosY < nextTilePosY
                then VisitedTile currentTile RightTurn (catMaybes [lookupTile (currentTilePosX + 1, currentTilePosY), lookupTile (currentTilePosX + 1, currentTilePosY - 1), lookupTile (currentTilePosX, currentTilePosY - 1)]) []
                else VisitedTile currentTile LeftTurn [] (catMaybes [lookupTile (currentTilePosX + 1, currentTilePosY), lookupTile (currentTilePosX + 1, currentTilePosY - 1), lookupTile (currentTilePosX, currentTilePosY - 1)])
            )
          Start -> asVisitedTile tileMap nextTile (Tile (inferType tileMap currentTile) (position currentTile))
      )

step :: TileMap -> (Int, [VisitedTile], Tile) -> [(Int, [VisitedTile], Tile)]
step tileMap (stepsTaken, previousTiles, currentTile) =
  let flowingTo = findFlowingTo tileMap currentTile
      out =
        filter
          ( \(_, _, nextTile) -> case previousTiles of
              previousTile : _ -> position (tile previousTile) /= position nextTile
              _ -> True
          )
          $ map (\nextTile -> (stepsTaken + 1, asVisitedTile tileMap nextTile currentTile : previousTiles, nextTile)) flowingTo
   in out

walkUntil :: TileMap -> (Tile -> Bool) -> Tile -> [(Int, [VisitedTile], Tile)]
walkUntil tileMap predicate startTile =
  let start = (0, [], startTile)
      walkUntil' (stepsTaken, previousTiles, currentTile) =
        let nextSteps = step tileMap (stepsTaken, previousTiles, currentTile)
            out =
              case nextSteps of
                [] -> [(stepsTaken, previousTiles, currentTile)]
                other -> do
                  (steps, visited, nextTile) <- other
                  if predicate nextTile
                    then [(steps, visited, nextTile)]
                    else walkUntil' (steps, visited, nextTile)
         in out
      out = walkUntil' start
   in out

isStart :: Tile -> Bool
isStart tile = tileType tile == Start

loopTiles :: TileMap -> [VisitedTile]
loopTiles tileMap =
  let start = findStart tileMap
      destinations = walkUntil tileMap isStart start
      (_, visitedTiles, currentTile) = maximumBy (\(steps1, _, _) (steps2, _, _) -> compare steps1 steps2) destinations
   in visitedTiles

adjacentTiles :: TileMap -> Tile -> [Tile]
adjacentTiles tileMap tile =
  let (tilePosX, tilePosY) = position tile
      adjacentPositions = [(tilePosX + 1, tilePosY), (tilePosX - 1, tilePosY), (tilePosX, tilePosY + 1), (tilePosX, tilePosY - 1)]
   in mapMaybe (tileByPosition tileMap) adjacentPositions

floodableTiles :: TileMap -> TileMap -> Tile -> [Tile]
floodableTiles entireMap loopMap tile =
  let touchingTiles = adjacentTiles entireMap tile
      out = filter (\tile -> not (position tile `Map.member` loopMap)) touchingTiles
   in out

flood :: TileMap -> TileMap -> [Tile] -> TileMap
flood entireMap loopMap startingTiles =
  let go :: TileMap -> [Tile] -> TileMap
      go seen [] = seen
      go seen tiles =
        let nextTiles = concatMap (filter (\tile -> not (position tile `Map.member` seen)) . floodableTiles entireMap loopMap) tiles
         in if null nextTiles
              then seen
              else go (Map.union seen (toTileMap tiles)) nextTiles
   in go Map.empty startingTiles

drawTile :: Tile -> String -> String -> String
drawTile tile bg fg =
  let (x, y) = position tile
   in case tileType tile of
        Main.Empty -> color bg fg " "
        Start -> color bg redFg "S"
        PipeNS -> color bg fg "║"
        PipeEW -> color bg fg "═"
        PipeNE -> color bg fg "╚"
        PipeNW -> color bg fg "╝"
        PipeSE -> color bg fg "╔"
        PipeSW -> color bg fg "╗"

color bgColor fgColor text = bgColor ++ fgColor ++ text ++ "\x1b[0m"

darkGrayBg = "\x1b[48;2;50;50;50m"

darkRedBg = "\x1b[48;2;200;50;50m"

darkBlueBg = "\x1b[48;2;50;50;200m"

darkGreenBg = "\x1b[48;2;50;200;50m"

redFg = "\x1b[38;2;255;0;0m"

greenFg = "\x1b[38;2;0;255;0m"

normalFg = "\x1b[39m"

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll "" contents of
        Right tileGrid ->
          let tileMap = toTileMap (concat tileGrid)
              start = findStart tileMap
              loop = loopTiles tileMap
              loopMap = toTileMap $ map tile loop
              leftTiles = nubBy (\x y -> position x == position y) $ concatMap (filter (\tile -> not (position tile `Map.member` loopMap)) . (\(VisitedTile _ _ left _) -> left)) loop
              leftTileMap = toTileMap leftTiles
              rightTiles = nubBy (\x y -> position x == position y) $ concatMap (filter (\tile -> not (position tile `Map.member` loopMap)) . (\(VisitedTile _ _ _ right) -> right)) loop
              rightTileMap = toTileMap rightTiles
              clockWise =
                foldr
                  ( \(VisitedTile _ turn _ _) acc ->
                      case turn of
                        LeftTurn -> acc - 1
                        RightTurn -> acc + 1
                        Straight -> acc
                  )
                  0
                  loop
                  > 0
              floodedMap = if clockWise then flood tileMap loopMap rightTiles else flood tileMap loopMap leftTiles
           in do
                mapM_
                  ( putStrLn
                      . concatMap
                        ( \tile ->
                            let tilePos = position tile
                                isLoop = isJust (tileByPosition loopMap tilePos)
                                isLeft = isJust (tileByPosition leftTileMap tilePos)
                                isRight = isJust (tileByPosition rightTileMap tilePos)
                                isFlooded = isJust (tileByPosition floodedMap tilePos)
                             in drawTile
                                  tile
                                  ( if isLoop
                                      then darkGrayBg
                                      else
                                        if isFlooded
                                          then darkGreenBg
                                          else
                                            if isLeft
                                              then if clockWise then darkBlueBg else darkRedBg
                                              else
                                                if isRight
                                                  then if clockWise then darkRedBg else darkBlueBg
                                                  else darkGrayBg
                                  )
                                  (if isLoop then greenFg else normalFg)
                        )
                  )
                  tileGrid
                print $ Map.size floodedMap
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
