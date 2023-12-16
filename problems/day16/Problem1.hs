import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment (getArgs)
import System.IO
import Text.Parsec hiding (Empty)
import Text.Parsec.String

data TileType = Empty | HSplit | VSplit | RMirror | LMirror deriving (Eq)

instance Show TileType where
  show Empty = "."
  show HSplit = "-"
  show VSplit = "|"
  show RMirror = "/"
  show LMirror = "\\"

data Tile = Tile {tpe :: !TileType, rayNorth :: !(Maybe Bool), rayEast :: !(Maybe Bool), raySouth :: !(Maybe Bool), rayWest :: !(Maybe Bool)}

instance Show Tile where
  show (Tile tpe rayNorth rayEast raySouth rayWest) = "[" ++ show tpe ++ n ++ e ++ s ++ w ++ "]"
    where
      n = case rayNorth of
        Just True -> "T"
        Just False -> "F"
        Nothing -> "_"
      e = case rayEast of
        Just True -> "T"
        Just False -> "F"
        Nothing -> "_"
      s = case raySouth of
        Just True -> "T"
        Just False -> "F"
        Nothing -> "_"
      w = case rayWest of
        Just True -> "T"
        Just False -> "F"
        Nothing -> "_"

data Ray = N | E | S | W deriving (Show)

parseTile :: Parser Tile
parseTile = do
  tpeChar <- oneOf ".-|/\\"
  case tpeChar of
    '.' -> return $ Tile Empty Nothing Nothing Nothing Nothing
    '-' -> return $ Tile HSplit (Just False) Nothing (Just False) Nothing
    '|' -> return $ Tile VSplit Nothing (Just False) Nothing (Just False)
    '/' -> return $ Tile RMirror Nothing Nothing Nothing Nothing
    '\\' -> return $ Tile LMirror Nothing Nothing Nothing Nothing

parseAll :: Parser (Map.Map (Int, Int) Tile)
parseAll = do
  listOfLists <- sepEndBy (many parseTile) newline
  let tiles = zipWith (\y row -> zipWith (\x tile -> ((x, y), tile)) [0 ..] row) [0 ..] listOfLists
  return $ Map.fromList $ concat tiles

defaultMaybeAndReturnIfSet :: Maybe a -> a -> (a, Bool)
defaultMaybeAndReturnIfSet maybeValue defaultValue = case maybeValue of
  Just value -> (value, False)
  Nothing -> (defaultValue, True)

propagateRay :: Tile -> Ray -> [Ray]
propagateRay (Tile Empty Nothing _ _ _) N = [N]
propagateRay (Tile Empty _ Nothing _ _) E = [E]
propagateRay (Tile Empty _ _ Nothing _) S = [S]
propagateRay (Tile Empty _ _ _ Nothing) W = [W]
propagateRay (Tile HSplit _ Nothing _ Nothing) N = [E, W]
propagateRay (Tile HSplit _ Nothing _ _) N = [E]
propagateRay (Tile HSplit _ _ _ Nothing) N = [W]
propagateRay (Tile HSplit _ Nothing _ Nothing) S = [E, W]
propagateRay (Tile HSplit _ Nothing _ _) S = [E]
propagateRay (Tile HSplit _ _ _ Nothing) S = [W]
propagateRay (Tile HSplit _ Nothing _ _) E = [E]
propagateRay (Tile HSplit _ _ _ Nothing) W = [W]
propagateRay (Tile HSplit _ _ _ _) _ = []
propagateRay (Tile VSplit Nothing _ Nothing _) E = [N, S]
propagateRay (Tile VSplit Nothing _ _ _) E = [N]
propagateRay (Tile VSplit _ _ Nothing _) E = [S]
propagateRay (Tile VSplit Nothing _ Nothing _) W = [N, S]
propagateRay (Tile VSplit Nothing _ _ _) W = [N]
propagateRay (Tile VSplit _ _ Nothing _) W = [S]
propagateRay (Tile VSplit Nothing _ _ _) N = [N]
propagateRay (Tile VSplit _ _ Nothing _) S = [S]
propagateRay (Tile VSplit _ _ _ _) _ = []
propagateRay (Tile LMirror Nothing _ _ _) W = [N]
propagateRay (Tile LMirror _ Nothing _ _) S = [E]
propagateRay (Tile LMirror _ _ Nothing _) E = [S]
propagateRay (Tile LMirror _ _ _ Nothing) N = [W]
propagateRay (Tile RMirror Nothing _ _ _) E = [N]
propagateRay (Tile RMirror _ Nothing _ _) N = [E]
propagateRay (Tile RMirror _ _ Nothing _) W = [S]
propagateRay (Tile RMirror _ _ _ Nothing) S = [W]

illuminateDirection :: Tile -> Ray -> Tile
illuminateDirection tile ray = case ray of
  N -> tile {rayNorth = Just True}
  E -> tile {rayEast = Just True}
  S -> tile {raySouth = Just True}
  W -> tile {rayWest = Just True}

mergeTiles :: Tile -> Tile -> Tile
mergeTiles old new = case old of
  Tile tpe n e s w ->
    let n' = asum [n, rayNorth new]
        e' = asum [e, rayEast new]
        s' = asum [s, raySouth new]
        w' = asum [w, rayWest new]
     in Tile tpe n' e' s' w'

propagateLight :: Map.Map (Int, Int) Tile -> [((Int, Int), Ray)] -> ([((Int, Int), Ray)], Map.Map (Int, Int) Tile)
propagateLight tiles [] = ([], tiles)
propagateLight tiles (((x, y), ray) : rest) = case Map.lookup (x, y) tiles of
  Just tile ->
    let newRays = propagateRay tile ray
        updatedTiles = map (illuminateDirection tile) newRays
        updatedMap = foldl' (flip (Map.insertWith mergeTiles (x, y))) tiles updatedTiles
        nextTiles =
          map
            ( \newRay -> case newRay of
                N -> ((x, y - 1), newRay)
                E -> ((x + 1, y), newRay)
                S -> ((x, y + 1), newRay)
                W -> ((x - 1, y), newRay)
            )
            newRays
     in propagateLight updatedMap (nextTiles ++ rest)
  Nothing -> propagateLight tiles rest

isEnergized :: Tile -> Bool
isEnergized (Tile _ n e s w) = or $ catMaybes [n, e, s, w]

countEnergized :: Map.Map (Int, Int) Tile -> Int
countEnergized = Map.size . Map.filter isEnergized

asRows :: Map.Map (Int, Int) Tile -> [[Tile]]
asRows inputMap =
  let rows = Map.toList inputMap
      maxRow = maximum (map (fst . fst) rows)
      maxCol = maximum (map (snd . fst) rows)
   in [[Map.findWithDefault (Tile Empty Nothing Nothing Nothing Nothing) (row, col) inputMap | row <- [0 .. maxCol]] | col <- [0 .. maxRow]]

mapToString :: Map.Map (Int, Int) Tile -> String
mapToString m = foldl' (\acc row -> acc ++ concatMap show row ++ "\n") "" $ asRows m

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right all -> do
          let finalMap = snd (propagateLight all [((0, 0), E)])
          print $ countEnergized finalMap
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
