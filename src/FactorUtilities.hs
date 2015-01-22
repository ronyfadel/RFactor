module FactorUtilities where

import Control.Monad
import FactorDefinitions
import FactorEval

-- takes file contents and adds vocabulary to State
addVocab :: String -> SystemState -> SystemState
addVocab contents systemState = foldl (\systemState line -> readEval line systemState) systemState (lines contents)

-- takes list of vocab filenames, reads corresponding file, adds vocab
addVocabFiles :: SystemState -> IO SystemState
addVocabFiles systemState = do
							newState <- addAllVocabs systemState
							return $ newState {newVocabs = []}
							where
								addAllVocabs state = foldl (\state filePath -> do
													contents <- readFile filePath
													liftM (addVocab contents) state
													) (return state) (newVocabs state)

-- prints all IO to stdout
showAllIO :: SystemState -> IO SystemState
showAllIO systemState = do
						foldl (\unit toPrint -> do putStrLn $ show toPrint) (return ()) (toIO systemState)
						return systemState {toIO = []}