import Data.Foldable
import Data.Functor
import Data.List as List
import Data.Map as Map
import Data.Maybe
import qualified Data.Ord as List
import Data.Type.Equality (apply)
import System.Environment (getArgs)
import System.IO
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number
import Text.Printf

data PartCategory = X | M | A | S deriving (Show, Eq, Ord)

data PartSpec = PartSpec {x :: (Int, Int), m :: (Int, Int), a :: (Int, Int), s :: (Int, Int)} deriving (Show, Eq, Ord)

data RuleCondition = GreaterThanCondition {partCategory :: PartCategory, threshold :: Int} | LessThanCondition {partCategory :: PartCategory, threshold :: Int} deriving (Show, Eq, Ord)

data RuleOutcome = SendToWorkflow WorkflowName | Reject | Accept deriving (Show, Eq, Ord)

data Rule = ConditionRule {ruleCondition :: RuleCondition, ruleOutcome :: RuleOutcome} | StaticRule {ruleOutcome :: RuleOutcome} deriving (Show)

newtype WorkflowName = WorkflowName String deriving (Show, Eq, Ord)

data Workflow = Workflow {worfklowName :: WorkflowName, rules :: [Rule]} deriving (Show)

parsePartCategory :: Parser PartCategory
parsePartCategory = do
  partCategory <- oneOf "xmas"
  return $ case partCategory of
    'x' -> X
    'm' -> M
    'a' -> A
    's' -> S

parseCondition :: Parser RuleCondition
parseCondition = do
  partCategory <- parsePartCategory
  do
    condition <- oneOf "<>"
    case condition of
      '<' -> do
        LessThanCondition partCategory <$> nat
      '>' -> do
        GreaterThanCondition partCategory <$> nat

parseRuleOutcome :: Parser RuleOutcome
parseRuleOutcome = do
  (char 'A' >> return Accept) <|> (char 'R' >> return Reject) <|> (many1 alphaNum >>= \x -> return (SendToWorkflow (WorkflowName x)))

parseConditionRule :: Parser Rule
parseConditionRule = do
  ruleCondition <- parseCondition
  char ':'
  ruleOutcome <- parseRuleOutcome
  return $ ConditionRule ruleCondition ruleOutcome

parseStaticRule :: Parser Rule
parseStaticRule = do
  ruleOutcome <- parseRuleOutcome
  return $ StaticRule ruleOutcome

parseWorkflow :: Parser Workflow
parseWorkflow = do
  workflowName <- many1 alphaNum
  char '{'
  rules <- sepEndBy1 (try parseConditionRule <|> parseStaticRule) (char ',')
  char '}'
  return $ Workflow (WorkflowName workflowName) rules

parseAll = sepEndBy1 (try parseWorkflow) (char '\n')

workflowsByName :: [Workflow] -> Map WorkflowName Workflow
workflowsByName workflows = Map.fromList $ List.map (\workflow -> (worfklowName workflow, workflow)) workflows

applyWorkflow :: Map WorkflowName Workflow -> WorkflowName -> PartSpec -> [PartSpec]
applyWorkflow workflowsMap workflowName partSpec =
  case Map.lookup workflowName workflowsMap of
    Nothing -> error $ "Workflow " ++ show workflowName ++ " not found"
    Just workflow -> fst $ go (rules workflow) (Just partSpec) []
  where
    go :: [Rule] -> Maybe PartSpec -> [PartSpec] -> ([PartSpec], Maybe PartSpec)
    go _ Nothing out = (out, Nothing)
    go [] (Just partSpec) out = (out ++ [partSpec], Nothing)
    go (rule : rules) (Just partSpec) out =
      let (newPartSpecs, newPartSpec) = applyRule workflowsMap rule partSpec
       in go rules newPartSpec (out ++ newPartSpecs)

applyRule :: Map WorkflowName Workflow -> Rule -> PartSpec -> ([PartSpec], Maybe PartSpec)
applyRule workflowsMap (ConditionRule (GreaterThanCondition X threshold) outcome) (PartSpec (xMin, xMax) mRange aRange sRange) =
  let okSpec = PartSpec (max xMin (threshold + 1), xMax) mRange aRange sRange
      koSpec = PartSpec (xMin, min xMax threshold) mRange aRange sRange
   in case outcome of
        Accept -> ([okSpec], Just koSpec)
        Reject -> ([], Just koSpec)
        SendToWorkflow workflowName -> (applyWorkflow workflowsMap workflowName okSpec, Just koSpec)
applyRule workflowsMap (ConditionRule (GreaterThanCondition M threshold) outcome) (PartSpec xRange (mMin, mMax) aRange sRange) =
  let okSpec = PartSpec xRange (max mMin (threshold + 1), mMax) aRange sRange
      koSpec = PartSpec xRange (mMin, min mMax threshold) aRange sRange
   in case outcome of
        Accept -> ([okSpec], Just koSpec)
        Reject -> ([], Just koSpec)
        SendToWorkflow workflowName -> (applyWorkflow workflowsMap workflowName okSpec, Just koSpec)
applyRule workflowsMap (ConditionRule (GreaterThanCondition A threshold) outcome) (PartSpec xRange mRange (aMin, aMax) sRange) =
  let okSpec = PartSpec xRange mRange (max aMin (threshold + 1), aMax) sRange
      koSpec = PartSpec xRange mRange (aMin, min aMax threshold) sRange
   in case outcome of
        Accept -> ([okSpec], Just koSpec)
        Reject -> ([], Just koSpec)
        SendToWorkflow workflowName -> (applyWorkflow workflowsMap workflowName okSpec, Just koSpec)
applyRule workflowsMap (ConditionRule (GreaterThanCondition S threshold) outcome) (PartSpec xRange mRange aRange (sMin, sMax)) =
  let okSpec = PartSpec xRange mRange aRange (max sMin (threshold + 1), sMax)
      koSpec = PartSpec xRange mRange aRange (sMin, min sMax threshold)
   in case outcome of
        Accept -> ([okSpec], Just koSpec)
        Reject -> ([], Just koSpec)
        SendToWorkflow workflowName -> (applyWorkflow workflowsMap workflowName okSpec, Just koSpec)
applyRule workflowsMap (ConditionRule (LessThanCondition X threshold) outcome) (PartSpec (xMin, xMax) mRange aRange sRange) =
  let okSpec = PartSpec (xMin, min xMax (threshold - 1)) mRange aRange sRange
      koSpec = PartSpec (max xMin threshold, xMax) mRange aRange sRange
   in case outcome of
        Accept -> ([okSpec], Just koSpec)
        Reject -> ([], Just koSpec)
        SendToWorkflow workflowName -> (applyWorkflow workflowsMap workflowName okSpec, Just koSpec)
applyRule workflowsMap (ConditionRule (LessThanCondition M threshold) outcome) (PartSpec xRange (mMin, mMax) aRange sRange) =
  let okSpec = PartSpec xRange (mMin, min mMax (threshold - 1)) aRange sRange
      koSpec = PartSpec xRange (max mMin threshold, mMax) aRange sRange
   in case outcome of
        Accept -> ([okSpec], Just koSpec)
        Reject -> ([], Just koSpec)
        SendToWorkflow workflowName -> (applyWorkflow workflowsMap workflowName okSpec, Just koSpec)
applyRule workflowsMap (ConditionRule (LessThanCondition A threshold) outcome) (PartSpec xRange mRange (aMin, aMax) sRange) =
  let okSpec = PartSpec xRange mRange (aMin, min aMax (threshold - 1)) sRange
      koSpec = PartSpec xRange mRange (max aMin threshold, aMax) sRange
   in case outcome of
        Accept -> ([okSpec], Just koSpec)
        Reject -> ([], Just koSpec)
        SendToWorkflow workflowName -> (applyWorkflow workflowsMap workflowName okSpec, Just koSpec)
applyRule workflowsMap (ConditionRule (LessThanCondition S threshold) outcome) (PartSpec xRange mRange aRange (sMin, sMax)) =
  let okSpec = PartSpec xRange mRange aRange (sMin, min sMax (threshold - 1))
      koSpec = PartSpec xRange mRange aRange (max sMin threshold, sMax)
   in case outcome of
        Accept -> ([okSpec], Just koSpec)
        Reject -> ([], Just koSpec)
        SendToWorkflow workflowName -> (applyWorkflow workflowsMap workflowName okSpec, Just koSpec)
applyRule workflowsMap (StaticRule Accept) partSpec = ([partSpec], Nothing)
applyRule workflowsMap (StaticRule Reject) partSpec = ([], Nothing)
applyRule workflowsMap (StaticRule (SendToWorkflow workflowName)) partSpec = (applyWorkflow workflowsMap workflowName partSpec, Nothing)

countPossibleParts :: PartSpec -> Int
countPossibleParts (PartSpec (xMin, xMax) (mMin, mMax) (aMin, aMax) (sMin, sMax)) = max 0 (xMax - xMin + 1) * max 0 (mMax - mMin + 1) * max 0 (aMax - aMin + 1) * max 0 (sMax - sMin + 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right parsed -> do
          let workflows = parsed
              workflowsMap = workflowsByName workflows
              finalPartSpecs = applyWorkflow workflowsMap (WorkflowName "in") (PartSpec (1, 4000) (1, 4000) (1, 4000) (1, 4000))
              possiblePartsCount = sum (List.map countPossibleParts finalPartSpecs)
          putStrLn $ printf "Possible parts count: %d" possiblePartsCount
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"