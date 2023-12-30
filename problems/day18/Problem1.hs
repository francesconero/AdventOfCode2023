import Data.Foldable
import Data.Functor
import Data.List as List
import Data.Map as Map
import Data.Maybe
import qualified Data.Ord as List
import Debug.Trace
import System.Environment (getArgs)
import System.IO
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number
import Text.Printf

data Direction = U | R | D | L deriving (Show, Eq)

data RGB = RGB {red :: Int, green :: Int, blue :: Int} deriving (Show, Eq)

data Instruction = Instruction {direction :: Direction, steps :: Int, rgb :: RGB} deriving (Show, Eq)

newtype Point = Point (Int, Int) deriving (Show, Eq)

newtype Segment = Segment (Point, Point) deriving (Show, Eq)

newtype ColoredSegment = ColoredSegment (Segment, RGB) deriving (Show, Eq)

parseDirection :: Parser Direction
parseDirection = do
  direction <- oneOf "URDL"
  return $ case direction of
    'U' -> U
    'R' -> R
    'D' -> D
    'L' -> L

parseSteps :: Parser Int
parseSteps = nat

parseHex :: Int -> Parser Int
parseHex n = do
  hex <- count n hexDigit
  case parse (try hexnum) "" hex of
    Left err -> fail (show err)
    Right val -> return val

parseRGB :: Parser RGB
parseRGB = do
  char '#'
  red <- parseHex 2
  green <- parseHex 2
  blue <- parseHex 2
  return $ RGB red green blue

parseInstruction :: Parser Instruction
parseInstruction = do
  direction <- parseDirection
  space
  steps <- parseSteps
  space
  char '('
  rgb <- parseRGB
  char ')'
  return $ Instruction direction steps rgb

parseAll = sepEndBy1 parseInstruction (char '\n')

dig :: Point -> Instruction -> ColoredSegment
dig (Point (x, y)) (Instruction direction steps rgb) = ColoredSegment (Segment (Point (startX, startY), Point (endX, endY)), rgb)
  where
    ((startX, startY), (endX, endY)) = case direction of
      U -> ((x, y - 1), (x, y - steps))
      R -> ((x + 1, y), (x + steps, y))
      D -> ((x, y + 1), (x, y + steps))
      L -> ((x - 1, y), (x - steps, y))

digAll :: [ColoredSegment] -> [Instruction] -> [ColoredSegment]
digAll segments [] = segments
digAll [] (i : is) = digAll [dig (Point (0, 0)) i] is
digAll ((ColoredSegment (Segment (p1, p2), c)) : ss) (i : is) = digAll (dig p2 i : ColoredSegment (Segment (p1, p2), c) : ss) is

signedDistance :: Point -> Segment -> Int
signedDistance (Point (xC, yC)) (Segment (Point (xA, yA), Point (xB, yB)))
  | xA == xB =
      let horizontalDist = xC - xA
          sideSign = if yB >= yA then 1 else -1
          yClosest
            | yC < min yA yB = min yA yB
            | yC > max yA yB = max yA yB
            | otherwise = yC
          verticalDist = yC - yClosest
          distSign
            | horizontalDist > 0 = 1
            | horizontalDist == 0 = if verticalDist > 0 then 1 else -1
            | otherwise = -1
       in if horizontalDist == 0
            then
              if verticalDist == 0
                then 0
                else 100000
            else sideSign * distSign * (abs horizontalDist + abs verticalDist)
  | yA == yB =
      let verticalDist = yC - yA
          sideSign = if xB >= xA then 1 else -1
          xClosest
            | xC < min xA xB = min xA xB
            | xC > max xA xB = max xA xB
            | otherwise = xC
          horizontalDist = xC - xClosest
          distSign
            | verticalDist > 0 = 1
            | verticalDist == 0 = if horizontalDist < 0 then 1 else -1
            | otherwise = -1
       in if verticalDist == 0
            then
              if horizontalDist == 0
                then 0
                else 100000
            else -sideSign * distSign * (abs verticalDist + abs horizontalDist)
  | otherwise = error "Segment is not aligned horizontally or vertically"

findClosest :: [ColoredSegment] -> Point -> ColoredSegment
findClosest segments point = List.minimumBy (List.comparing (\(ColoredSegment (segment, _)) -> abs $ signedDistance point segment)) segments

boundingBox :: [Point] -> (Point, Point)
boundingBox [] = (Point (0, 0), Point (0, 0))
boundingBox [p] = (p, p)
boundingBox (Point (x1, y1) : ps) =
  let (Point (minX, minY), Point (maxX, maxY)) = boundingBox ps
   in (Point (min x1 minX, min y1 minY), Point (max x1 maxX, max y1 maxY))

drawPoint :: [ColoredSegment] -> Point -> String
drawPoint boundaries p = case findClosest boundaries p of
  ColoredSegment (s, RGB r g b) ->
    let distance = signedDistance p s
        normalized = min 255 (max 0 ((distance * 2) + 128))
        symbol = if distance == 0 then "#" else if distance > 0 then "+" else "-"
        w = printf "\x1b[48;2;%d;%d;%dm\x1b[38;2;%d;%d;%dm%s\x1b[0m" normalized normalized normalized r g b symbol
     in w

drawMap :: [ColoredSegment] -> [String]
drawMap boundaries =
  let (Point (minX, minY), Point (maxX, maxY)) = boundingBox (concatMap (\(ColoredSegment (Segment (p1, p2), _)) -> [p1, p2]) boundaries)
   in [concat [drawPoint boundaries (Point (x, y)) | x <- [minX .. maxX]] | y <- [minY .. maxY]]

countPoint :: [ColoredSegment] -> Point -> Int
countPoint boundaries p = case findClosest boundaries p of
  ColoredSegment (s, RGB r g b) ->
    let distance = signedDistance p s
     in if distance <= 0 then 1 else 0

countMap :: [ColoredSegment] -> Int
countMap boundaries =
  let (Point (minX, minY), Point (maxX, maxY)) = boundingBox (concatMap (\(ColoredSegment (Segment (p1, p2), _)) -> [p1, p2]) boundaries)
   in sum [sum [countPoint boundaries (Point (x, y)) | x <- [minX .. maxX]] | y <- [minY .. maxY]]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right parsed -> do
          let dug = digAll [] parsed
          mapM_ putStrLn $ drawMap dug
          print $ countMap dug
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
