import System.IO
import FactorParsing
import FactorEval
import FactorDefinitions
import FactorUtilities
import Control.Monad

repl :: SystemState -> IO ()
repl systemState = do
					putStr "> "
					hFlush stdout
					x <- getLine
					let newSystemState = readEval x systemState in do
						ns <-  (return newSystemState) >>= addVocabFiles >>= showAllIO
						-- ns <- addVocabFiles newSystemState
						-- ns <- showAllIO ns
						-- putStr $ show ns
						putStr $ show (state ns)
						repl ns

main :: IO ()
main = do
		putStr "\ESC[2J"
		putStrLn "Welcome to RFactor 0.3b"
		repl emptySystemState