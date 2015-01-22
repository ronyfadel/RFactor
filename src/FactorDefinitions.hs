module FactorDefinitions where
import Data.List

-- http://docs.factorcode.org/content/article-conventions.html
-- http://docs.factorcode.org/content/article-syntax-literals.html

data SystemState = SystemState {state :: State, newVocabs :: [String], toIO :: [FTerm]} deriving (Show)

type Stack = [FTerm]
type Definitions = [(String, Definition)]
data Definition = WordDefinition {stackEffect :: FTerm, body :: [FTerm]} | ClassDefinition { ivars :: [String] } deriving (Eq, Show)
data State = State {stack :: Stack, definitions :: Definitions}

data FTerm = FNumber Integer
	| FString String
	| FBoolean Bool
	| FWord String
	| FGetter String
	| FSetter String
	| FQuotation [FTerm]
	| FArray [FTerm]
	| FHashtable [(FTerm, FTerm)]
	| FStackEffect ([String], [String])
	| FTuple (String, [FTerm])
	deriving (Eq)

instance Show FTerm where
	show (FNumber num) = show num
	show (FString contents) = "\"" ++ contents ++ "\""
	show (FBoolean True) = "t"
	show (FBoolean False) = "f"
	show (FWord word) = word
	show (FQuotation quot') = "[ " ++ (intercalate " " (map show quot')) ++ " ]"
	show (FArray a) = "{ " ++ (intercalate " " (map show a)) ++ " }"
	show (FHashtable h) = "H{ " ++ (intercalate " " (map (\(k,v) -> "{ " ++ (show k) ++ " " ++ (show v) ++ " }") h)) ++ " }"
	show (FStackEffect (in', out')) = "( " ++ (intercalate " " in') ++ " -- " ++ (intercalate " " out') ++ " )"
	show (FTuple (name, ivars)) = "T{ " ++ name ++ " " ++ (intercalate " " (map show ivars)) ++ " }"
	show (FGetter g) = g ++ ">>"
	show (FSetter s) = s ++ "<<"

boolValue :: FTerm -> Bool
boolValue (FBoolean False) = False
boolValue _ = True

globalStackEffect :: FTerm -> Int
globalStackEffect (FStackEffect (in', out')) = (length out') - (length in')

-- instance Show Definition where
-- 	show (WordDefinition stackEffect body) = (show stackEffect) ++ " " ++ (show body)
-- 	show (ClassDefinition ivars) = show ivars

instance Show State where
	show (State stack def) =  "--- Definitions:" ++ (showDefinitions def) ++ "\n--- Data stack:" ++ (showStack stack) ++ "\n"

showStack :: Stack -> String
showStack [] = ""
showStack (x:xs) = showStack xs ++ "\n" ++ (show x)

showDefinitions :: Definitions -> String
showDefinitions [] = ""
showDefinitions ( (name, def) : xs) = showDefinitions xs ++ "\n" ++ (name ++ " " ++ (show def))

emptyState :: State
emptyState = State [] []

emptySystemState :: SystemState
emptySystemState = SystemState emptyState [] []

withStack :: SystemState -> Stack -> SystemState
withStack systemState@(SystemState { state = state' }) stack' = systemState { state = state' {stack = stack'} }

getStack :: SystemState -> Stack
getStack = stack . state

topStack :: SystemState -> FTerm
topStack = head . getStack

tailStack :: SystemState -> Stack
tailStack = tail . getStack

popStack :: SystemState -> SystemState
popStack ss = withStack ss (tailStack ss)

pushStack :: SystemState -> FTerm -> SystemState
pushStack systemState term = withStack systemState (term : (stack . state $ systemState) )



