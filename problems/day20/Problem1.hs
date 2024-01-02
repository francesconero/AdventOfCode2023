import Data.Foldable as Foldable
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Sequence as Sequence
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Text.Printf

data Pulse = High | Low deriving (Show, Eq, Ord)

newtype ModuleName = ModuleName String deriving (Show, Eq, Ord)

data Module = FlipFlop Bool | Conjuction (Map ModuleName Pulse) | Broadcaster | Output ModuleName deriving (Show, Eq, Ord)

sendPulse :: Module -> ModuleName -> Pulse -> (Module, Maybe Pulse)
sendPulse (FlipFlop state) _ High = (FlipFlop state, Nothing)
sendPulse (FlipFlop True) _ Low = (FlipFlop False, Just Low)
sendPulse (FlipFlop False) _ Low = (FlipFlop True, Just High)
sendPulse (Conjuction state) from pulse =
  let newState = Map.insert from pulse state
   in if all (== High) (elems newState) then (Conjuction newState, Just Low) else (Conjuction newState, Just High)
sendPulse Broadcaster _ pulse = (Broadcaster, Just pulse)
sendPulse (Output name) _ pulse = (Output name, Nothing)

data ModuleBP = FlipFlopBp ModuleName [ModuleName] | ConjuctionBp ModuleName [ModuleName] | BroadcasterBp ModuleName [ModuleName] | OutputBp ModuleName deriving (Show, Eq, Ord)

parseFlipFlopBp :: Parser ModuleBP
parseFlipFlopBp = do
  char '%'
  name <- many1 letter
  string " -> "
  outputs <- sepBy1 (many1 letter) (string ", ")
  return $ FlipFlopBp (ModuleName name) (fmap ModuleName outputs)

parseConjuctionBp :: Parser ModuleBP
parseConjuctionBp = do
  char '&'
  name <- many1 letter
  string " -> "
  outputs <- sepBy1 (many1 letter) (string ", ")
  return $ ConjuctionBp (ModuleName name) (fmap ModuleName outputs)

parseBroadcasterBp :: Parser ModuleBP
parseBroadcasterBp = do
  string "broadcaster"
  string " -> "
  outputs <- sepBy1 (many1 letter) (string ", ")
  return $ BroadcasterBp (ModuleName "broadcaster") (fmap ModuleName outputs)

parseOutputBp :: Parser ModuleBP
parseOutputBp = do
  name <- many1 letter
  return $ OutputBp (ModuleName name)

parseAll :: Parser [ModuleBP]
parseAll = sepEndBy1 (parseFlipFlopBp <|> parseConjuctionBp <|> parseBroadcasterBp <|> parseOutputBp) (char '\n')

buildOutputMap :: [ModuleBP] -> Map ModuleName [ModuleName]
buildOutputMap =
  List.foldl'
    ( \acc bp -> case bp of
        FlipFlopBp name outputs -> Map.insert name outputs acc
        ConjuctionBp name outputs -> Map.insert name outputs acc
        BroadcasterBp name outputs -> Map.insert name outputs acc
        OutputBp name -> Map.insert name [] acc
    )
    Map.empty

invertMap :: Map ModuleName [ModuleName] -> Map ModuleName [ModuleName]
invertMap = Map.foldlWithKey' (\acc key values -> List.foldl' (\acc value -> Map.insertWith (++) value [key] acc) acc values) Map.empty

buildCircuit :: [ModuleBP] -> Map ModuleName [ModuleName] -> Map ModuleName Module
buildCircuit bps inputMap =
  let inputs =
        Map.fromList $
          fmap
            ( \bp -> case bp of
                FlipFlopBp name _ -> (name, FlipFlop False)
                ConjuctionBp name _ -> (name, Conjuction $ Map.fromList $ List.zip (inputMap ! name) (repeat Low))
                BroadcasterBp name _ -> (name, Broadcaster)
                OutputBp name -> (name, Output name)
            )
            bps
      outputs = Map.fromList $ fmap (\moduleName -> (moduleName, Output moduleName)) (Map.keys inputMap)
   in Map.union inputs outputs

processPulse :: Map ModuleName Module -> Map ModuleName [ModuleName] -> (ModuleName, ModuleName, Pulse) -> (Map ModuleName Module, [(ModuleName, ModuleName, Pulse)])
processPulse circuit outputMap (from, to, pulse) =
  let (newMdl, newPulse) = sendPulse (circuit ! to) from pulse
      newCircuit = Map.insert to newMdl circuit
      newPulses = Maybe.mapMaybe (\outputMdl -> fmap (\p -> (to, outputMdl, p)) newPulse) (Map.findWithDefault [] to outputMap)
   in (newCircuit, newPulses)

processPulses :: Map ModuleName Module -> Map ModuleName [ModuleName] -> Seq (ModuleName, ModuleName, Pulse) -> (Map ModuleName Module, [(ModuleName, ModuleName, Pulse)])
processPulses circuit outputMap pulses = go circuit outputMap pulses (Foldable.toList pulses)
  where
    go circuit outputMap pulses acc =
      case viewl pulses of
        EmptyL -> (circuit, acc)
        (from, to, pulse) :< rest ->
          let (newCircuit, newPulses) = processPulse circuit outputMap (from, to, pulse)
           in go newCircuit outputMap (rest >< Sequence.fromList newPulses) (acc ++ newPulses)

pressButton :: Map ModuleName Module -> Map ModuleName [ModuleName] -> (Map ModuleName Module, [(ModuleName, ModuleName, Pulse)])
pressButton circuit outputMap = processPulses circuit outputMap (Sequence.fromList [(ModuleName "button", ModuleName "broadcaster", Low)])

pressButtonNTimes :: Map ModuleName Module -> Map ModuleName [ModuleName] -> Int -> (Map ModuleName Module, [(ModuleName, ModuleName, Pulse)])
pressButtonNTimes circuit outputMap n = go circuit outputMap n []
  where
    go circuit outputMap 0 acc = (circuit, acc)
    go circuit outputMap n acc =
      let (newCircuit, newPulses) = pressButton circuit outputMap
       in go newCircuit outputMap (n - 1) (acc ++ newPulses)

result :: [(ModuleName, ModuleName, Pulse)] -> (Int, Int)
result = List.foldl' f (0, 0)
  where
    f (low, high) (_, _, Low) = (low + 1, high)
    f (low, high) (_, _, High) = (low, high + 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right parsed -> do
          let outputMap = buildOutputMap parsed
          let inputMap = invertMap outputMap
          let circuit = buildCircuit parsed inputMap
          let (newCircuit, pulses) = pressButtonNTimes circuit outputMap 1000
          let (lows, highs) = result pulses
          putStrLn (printf "Pulses: %s" (show (lows, highs)))
          putStrLn (printf "Result: %s" (show (lows * highs)))
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"