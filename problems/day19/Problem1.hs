import Data.Foldable
import Data.Functor
import Data.List as List
import Data.Map as Map
import Data.Maybe
import qualified Data.Ord as List
import System.Environment (getArgs)
import System.IO
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number
import Text.Printf

data PartCategory = X | M | A | S deriving (Show, Eq, Ord)

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show, Eq, Ord)

data RuleCondition = DynamicCondition {partCategory :: PartCategory, condition :: Int -> Bool}

instance Show RuleCondition where
  show (DynamicCondition partCategory condition) = printf "Dynamic condition %s" (show partCategory)

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
  condition <- do
    condition <- oneOf "<>"
    case condition of
      '<' -> do
        value <- nat
        return (<= value)
      '>' -> do
        value <- nat
        return (>= value)
  return $ DynamicCondition partCategory condition

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

parsePart :: Parser Part
parsePart = do
  string "{x="
  x <- nat
  string ",m="
  m <- nat
  string ",a="
  a <- nat
  string ",s="
  s <- nat
  char '}'
  return $ Part x m a s

parseAll = do
  workflows <- sepEndBy1 (try parseWorkflow) (char '\n')
  char '\n'
  parts <- sepEndBy1 parsePart (char '\n')
  return (workflows, parts)

workflowsByName :: [Workflow] -> Map WorkflowName Workflow
workflowsByName workflows = Map.fromList $ List.map (\workflow -> (worfklowName workflow, workflow)) workflows

applyRule :: Part -> Rule -> Maybe RuleOutcome
applyRule Part {x = x, m = m, a = a, s = s} (ConditionRule (DynamicCondition partCategory condition) ruleOutcome) =
  if condition
    ( case partCategory of
        X -> x
        M -> m
        A -> a
        S -> s
    )
    then Just ruleOutcome
    else Nothing
applyRule _ (StaticRule ruleOutcome) = Just ruleOutcome

applyFirstMatchingRule :: Part -> [Rule] -> RuleOutcome
applyFirstMatchingRule part rules =
  case List.find isJust (List.map (applyRule part) rules) of
    Just (Just ruleOutcome) -> ruleOutcome
    _ -> error "No matching rule"

processPart :: Map WorkflowName Workflow -> WorkflowName -> Part -> Bool
processPart workflows workflowName part =
  case Map.lookup workflowName workflows of
    Just workflow -> case applyFirstMatchingRule part (rules workflow) of
      Accept -> True
      Reject -> False
      SendToWorkflow toSendWorkflowName -> processPart workflows toSendWorkflowName part
    Nothing -> error (printf "No workflow found: %s" (show workflowName))

partitionParts :: Map WorkflowName Workflow -> [Part] -> ([Part], [Part])
partitionParts workflows = List.partition (processPart workflows (WorkflowName "in"))

partRating :: Part -> Int
partRating Part {x = x, m = m, a = a, s = s} = x + m + a + s

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parse parseAll filePath contents of
        Right parsed -> do
          let (workflows, parts) = parsed
              workflowsMap = workflowsByName workflows
              (acceptedParts, rejectedParts) = partitionParts workflowsMap parts
              acceptedRating = sum (List.map partRating acceptedParts)
          print workflowsMap
          putStrLn $ printf "Accepted rating: %d" acceptedRating
        Left error -> print error
    _ -> putStrLn "Usage: stack run -- ./input_file"
