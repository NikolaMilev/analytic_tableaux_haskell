module Parser where
	import Formula
	import NNF
	-- source
	-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

	-- better source: https://wiki.haskell.org/Parsing_a_simple_imperative_language
	import Control.Monad (void)
	import Text.Megaparsec
	import Text.Megaparsec.Expr
	import Text.Megaparsec.String -- input stream is of type ‘String’
	import qualified Text.Megaparsec.Lexer as L
	import Text.Megaparsec.Char

	spaceConsumer :: Parser ()
	spaceConsumer  = L.space (void spaceChar) lineComment blockComment
		where lineComment = L.skipLineComment "//"
		      blockComment = L.skipBlockComment "/*" "*/"
    -- probaj samo ovo ispod, ne trebaju ti komentari
	-- ovo ispod ne radi
	--spaceConsumer = void spaceChar


	operators :: [[Operator Parser Formula]]
	operators =
	  [ [Prefix (Not <$ symbol "~") ]
	  , [ InfixL (And <$ symbol "/\\")
	    , InfixL (Or <$ symbol "\\/") ],
	    [ InfixL (Impl   <$ symbol "=>") ]
	    ,[ InfixL (Eqv <$ symbol "<=>") ]
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
	chararcter :: Parser Char
	chararcter = L.charLiteral

	identifier :: Parser String
	identifier = many letterChar

	-- The list of reserved words
	--reservedWords :: [String] 
	--reservedWords = ["~", "\\/", "/\\", "<=>", "=>", "T","F","SAT","TAUT"]

	rword :: String -> Parser ()
	rword w = string w *> spaceConsumer


	mainParser :: Parser Formula
	mainParser = between spaceConsumer eof formulaParser

	--statement :: Parser 

	--formula :: Parser Formula
	--formula = parens formula <|> equivalence <|> implication <|> conjunction <|> disjunction <|> negation <|> atom <|> true <|> false

	formulaParser :: Parser Formula
	formulaParser = makeExprParser term operators

	-- the order is quite important!
	term :: Parser Formula
	term = parens formulaParser 
		<|> (rword "T" *> pure FTrue)
		<|> (rword "F" *> pure FFalse)
		<|> Atom <$> identifier


	parseString :: String -> Formula
	parseString str =
		case parse mainParser "" str of
		Left e  -> error $ show e
		Right r -> nnf r