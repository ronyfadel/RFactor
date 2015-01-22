module FactorEval where
import FactorDefinitions
import FactorParsing
import FactorPrimitives
import Data.List

readEval :: String -> SystemState -> SystemState
readEval s ss = evalWrapper (parseLine s) ss

evalWrapper :: ParsedLine -> SystemState -> SystemState
evalWrapper (ParseError err) systemState = addError err systemState
evalWrapper (ParsedVocabUse vocab) systemState = systemState {newVocabs = vocab : (newVocabs systemState)}
evalWrapper parsedLine systemState = eval parsedLine systemState

addError :: String -> SystemState -> SystemState
addError err systemState = systemState {toIO = (FString ("##################\nParsing error: " ++ err ++ "\n##################")) : toIO systemState }

-----

eval :: ParsedLine -> SystemState -> SystemState
eval (ParsedSentence terms) systemState = evalTerms terms systemState
eval (ParsedDefinition def) systemState	= systemState {state = pushDefinition def $ state systemState }
eval (ParsedComment _) systemState = systemState
eval _ systemState = undefined

evalTerms :: [FTerm] -> SystemState -> SystemState
evalTerms = flip $ foldl (\state term -> evalTerm term state)

checkTuple :: FTerm -> SystemState -> Bool
checkTuple (FTuple (className, ivars')) systemState = maybe (False)
									                  (\x -> length (ivars x) == length ivars')
									                  (lookup className (definitions . state $ systemState))

evalTerm :: FTerm -> SystemState -> SystemState
evalTerm (FWord ".") systemState = popStack $ systemState {toIO = (topStack systemState) : toIO systemState}
evalTerm (FWord word) systemState = call word systemState
evalTerm g@(FGetter getter) systemState = pushOnStack (get g systemState) (popStack systemState)
evalTerm g@(FSetter setter) systemState = pushOnStack (set g systemState) (popStack . popStack $ systemState)
evalTerm tuple@(FTuple _) systemState = if (checkTuple tuple systemState)
										then pushOnStack tuple systemState
										else addError "Bad Tuple, check definition" systemState
evalTerm term systemState = pushOnStack term systemState

pushOnStack :: FTerm -> SystemState -> SystemState
pushOnStack term systemState = withStack systemState (term : (getStack systemState))

pushDefinition :: (String, Definition) -> State -> State
pushDefinition (defname, def) state = state {definitions = (defname, def) : (definitions state) }

call :: String -> SystemState -> SystemState
call word systemState = maybe (callPrimitive word systemState (stack . state $ systemState))
	                  (\x ->  evalTerms (body x) systemState)
	                  (lookup word $ (definitions . state $ systemState))

callPrimitive :: String -> SystemState -> Stack -> SystemState
callPrimitive "call" systemState ((FQuotation x):xs) = evalTerms x $ withStack systemState xs
callPrimitive "if" systemState (falsequot:truequot:cond:xs) = callPrimitive "call" newSystemState (stack . state $ newSystemState)
															where
																toCallQuot = case boolValue(cond) of
																	True -> truequot
																	False -> falsequot
																newSystemState = withStack systemState (toCallQuot:xs)

callPrimitive "dip" systemState (x:y:xs) = pushOnStack y call_res
										where
											newSystemState = withStack systemState (x:xs)
											call_res = callPrimitive "call" newSystemState (stack . state $ newSystemState)


callPrimitive word systemState stack = withStack systemState (callPrimitive' word stack)

-- to rewrite --> written while super tired
get :: FTerm -> SystemState -> FTerm
get g@(FGetter getter) systemState = getWithTuple g (topStack systemState) systemState

getWithTuple :: FTerm -> FTerm -> SystemState  -> FTerm
getWithTuple (FGetter getter) (FTuple (className, ivars')) systemState = maybe (error "Class not found")
                                                               (\x -> maybe (error "Accessor not found") (\y -> ivars' !! y) (elemIndex getter (ivars x)))
                                                               (lookup className (definitions . state $ systemState))

set :: FTerm -> SystemState -> FTerm
set s@(FSetter setter) ss = setWithTuple s (topStack ss) (topStack . popStack $ ss) ss

setWithTuple :: FTerm -> FTerm -> FTerm -> SystemState -> FTerm
setWithTuple (FSetter setter) term (FTuple (className, ivars')) systemState = maybe (error "Class not found")
			                                                               (\x -> maybe (error "Accessor not found") (\y -> (FTuple (className, replaceNth y term ivars'))) (elemIndex setter (ivars x)))
			                                                               (lookup className (definitions . state $ systemState))
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs









