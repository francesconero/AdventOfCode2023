import Data.Foldable
import Data.List (partition)
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.ParserCombinators.Parsec.Number (nat)

data Cell = Cell
  { posX :: !Int,
    posY :: !Int
  }
  deriving (Eq, Ord, Show)

data Element = IntElement !Int | SymbolElement deriving (Show)

data PositionedElement = PositionedElement
  { value :: !Element,
    startCell :: !Cell,
    endCell :: !Cell
  }
  deriving (Show)

positionedElementParser = do
  startPos <- getPosition
  let startCell = Cell (sourceColumn startPos) (sourceLine startPos)
  element <- fmap IntElement nat <|> (char '*' >> return SymbolElement)
  endPos <- getPosition
  let endCell = Cell (sourceColumn endPos - 1) (sourceLine endPos)
  return (PositionedElement element startCell endCell)

emptyParser = many (noneOf "1234567890*")

touchesBefore source target =
  let sourceStartX = posX $ startCell source
      sourceEndX = posX $ endCell source
      sourceStartY = posY $ startCell source
      sourceEndY = posY $ endCell source
      targetStartX = posX $ startCell target
      targetEndX = posX $ endCell target
      targetStartY = posY $ startCell target
      targetEndY = posY $ endCell target
   in ( if (sourceStartY > targetEndY + 1)
          then False
          else
            if (sourceStartY == targetEndY)
              then
                if (sourceStartX - 1 <= targetEndX)
                  then True
                  else
                    if (sourceEndY == sourceStartY)
                      then
                        if (sourceStartY == targetStartY)
                          then False
                          else
                            if (sourceStartY - 1 == targetStartY)
                              then sourceEndX + 1 >= targetStartX
                              else True
                      else True
              else
                if (sourceStartY - 1 == targetStartY)
                  then not (sourceStartX - 1 > targetEndX || sourceEndX + 1 < targetStartX)
                  else sourceStartX - 1 <= targetEndX
      )

handleNextElement :: (Map.Map Cell [Int], [PositionedElement]) -> PositionedElement -> (Map.Map Cell [Int], [PositionedElement])
handleNextElement (output, existingElements) next =
  let (symbols, parts) =
        partition
          ( \x -> case value x of
              IntElement _ -> False
              SymbolElement -> True
          )
          existingElements
   in case value next of
        IntElement value ->
          let touching = filter (touchesBefore next) symbols
           in if null touching
                then (output, existingElements ++ [next])
                else (foldl' (\acc gear -> Map.insertWith (++) (startCell gear) [value] acc) output touching, existingElements)
        SymbolElement ->
          let (touching, notTouching) = partition (touchesBefore next) parts
           in ( foldl'
                  ( \acc part ->
                      Map.insertWith
                        (++)
                        (startCell next)
                        ( case value part of
                            IntElement value -> [value]
                            SymbolElement -> []
                        )
                        acc
                  )
                  output
                  touching,
                symbols ++ [next] ++ notTouching
              )

main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse (emptyParser *> sepEndBy positionedElementParser emptyParser) "" contents of
        Right elements -> print $ foldl (\acc x -> if length x == 2 then acc + foldl' (*) 1 x else acc) 0 (fst $ foldl' handleNextElement (Map.empty, []) elements)
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"