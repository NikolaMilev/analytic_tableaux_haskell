module Parser where
	import Formula

	-- source
	-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

	import Control.Monad (void)
	import Text.Megaparsec
	import Text.Megaparsec.Expr
	import Text.Megaparsec.String -- input stream is of type ‘String’
	import qualified Text.Megaparsec.Lexer as L

	spaceConsumer :: Parser ()
	spaceConsumer  = L.space (void spaceChar) lineComment blockComment
		where lineComment = L.skipLineComment "//"
		      blockComment = L.skipBlockComment "/*" "*/"
    -- probaj samo ovo ispod, ne trebaju ti komentari
	--spaceConsumer = void spaceChar


	aOperators :: [[Operator Parser AExpr]]
	aOperators =
	  [ [Prefix (Not <$ symbol "~") ]
	  , [ InfixL (And <$ symbol "/\\")
	    , InfixL (Or <$ symbol "\\/") ],
	    [ InfixL (Eqv <$ symbol "<=>")
	    , InfixL (Impl   <$ symbol "=>") ]
	  ]
	-- how to parse a lexeme
	-- L.lexeme wraps the lexeme parser with a space consumer
	lexeme :: Parser a -> Parser a
	lexeme = L.lexeme spaceConsumer

	-- how to parse one symbol
	symbol :: String -> Parser String
	symbol = L.symbol spaceConsumer

	parens :: Parser a -> Parser a
	parens = between (symbol "(") (symbol ")")

	-- how to parse a single character from stream
	char :: Parser Char
	char = L.charLiteral

	-- The list of reserved words
	reservedWords :: [String] 
	reservedWords = ["~", "\\/", "/\\", "<=>", "=>", "T","F","SAT","TAUT"]

	rword :: String -> Parser ()
	rword w = string w *> spaceConsumer


	mainParser :: Parser Formula
	mainParser = between spaceConsumer eof statement

	statement :: Parser 

	--formula :: Parser Formula
	--formula = parens formula <|> equivalence <|> implication <|> conjunction <|> disjunction <|> negation <|> atom <|> true <|> false


