module FactorParsing where
import FactorDefinitions
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data ParsedLine = ParsedSentence { getTerms :: [FTerm] }
				| ParsedDefinition (String, Definition)
				| ParsedVocabUse String
				| ParsedComment String
				| ParsedObject (String, Definition)
				| ParseError String

parseLine :: String -> ParsedLine
parseLine input = case parse parseInput "Factor" input of
	Left err -> ParseError $ show err
	Right val -> val

symbol :: Parser Char
symbol = oneOf "#$%&|*+-/<=>?@^_~'."

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser FTerm
parseAtom = do
			first <- letter <|> symbol
			rest <- many (letter <|> symbol <|> digit)
			let atom = first:rest
			return $ case (reverse atom) of 
				"t" -> FBoolean True
				"f" -> FBoolean False
				('>':'>':xs) -> FGetter $ reverse xs
				('<':'<':xs) -> FSetter $ reverse xs
				_    -> FWord atom

parseString :: Parser FTerm
parseString = do
				char '"'
				x <- many (noneOf "\"")
				char '"'
				return $ FString x

parseNumber :: Parser FTerm
parseNumber = do
				x <- many1 digit
				return $ FNumber (read x :: Integer)

parseQuotation :: Parser FTerm
parseQuotation = do
				string "[ "
				x <- endBy parseWord spaces
				string "]"
				return $ FQuotation x

parseArray :: Parser FTerm
parseArray = do
				string "{ "
				x <- endBy parseWord spaces
				string "}"
				return $ FArray x

parseKeyValuePair :: Parser (FTerm, FTerm)
parseKeyValuePair = do
				string "{ "
				x <- endBy parseWord spaces
				string "}"
				return (x !! 0, x !! 1)

parseHashtable :: Parser FTerm
parseHashtable = do
				string "H{ "
				x <- endBy parseKeyValuePair spaces
				string "}"
				return $ FHashtable x

parseStackEffect :: Parser FTerm
parseStackEffect = do
				string "( "
				in' <- endBy (many letter) spaces
				string "--"
				out' <- endBy (many letter) spaces
				string ")"
				return $ FStackEffect (in', out')

parseTuple :: Parser FTerm
parseTuple = do
				string "T{ "
				className <- many1 (letter <|> symbol <|> digit)
				spaces
				ivars <- endBy parseWord spaces
				string "}"
				return $ FTuple (className, ivars)

parseInput :: Parser ParsedLine
parseInput = try parseUseVocab <|> try parseDefinitions <|> parseComment <|> parseSentence

--a sentence is composed of many terms
parseSentence :: Parser ParsedLine
parseSentence = do
				x <- sepBy parseWord spaces
				return $ ParsedSentence x

parseWord :: Parser FTerm
parseWord = parseNumber
		 <|> parseHashtable
         <|> parseString
         <|> parseQuotation
         <|> parseArray
         <|> parseTuple
         <|> parseAtom
         <|> parseStackEffect

parseComment :: Parser ParsedLine
parseComment = do
				try (string "! ") <|> (string "#!")
				x <- many letter
				return $ ParsedComment x


parseDefinitions :: Parser ParsedLine
parseDefinitions = parseWordDef <|> parseClassDef

parseWordDef :: Parser ParsedLine
parseWordDef = do
				string ": "
				wordName <- many1 (letter <|> symbol <|> digit)
				spaces
				stackEffect <- parseStackEffect
				spaces
				body <- endBy parseWord spaces
				string ";"
				return $ ParsedDefinition (wordName, WordDefinition stackEffect body)

parseClassDef :: Parser ParsedLine
parseClassDef = do
				string "TUPLE: "
				className <- many1 (letter <|> symbol <|> digit)
				spaces
				ivars <- endBy (many letter) spaces
				string ";"
				return $ ParsedDefinition (className, ClassDefinition ivars)

parseUseVocab :: Parser ParsedLine
parseUseVocab = do
				string "USE: "
				vocab <- many (noneOf "")
				return $ ParsedVocabUse vocab





