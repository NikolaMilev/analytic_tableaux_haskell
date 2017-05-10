module Parser where
	import Formula
	import NNF
	import AnalyticTableaux

	import System.IO
	import Control.Monad
	import Text.ParserCombinators.Parsec
	import Text.ParserCombinators.Parsec.Expr
	import Text.ParserCombinators.Parsec.Language
	import qualified Text.ParserCombinators.Parsec.Token as Token
	

	-- tutorial for parsing: https://wiki.haskell.org/Parsing_a_simple_imperative_language
	
	-- the data structure representing one statement within our interpreter
	-- the statement is keyword 'taut' or 'sat' followed by a formula
	data Statement =  Tautology Formula
					| Satisfiable Formula 
					deriving (Eq)
	
	-- evaluation of the Statement
	evaluate :: Statement -> Bool
 	evaluate (Satisfiable formula) = is_satisfiable formula
   	evaluate (Tautology formula) = is_tautology formula

   	-- printing of the Statement
   	instance Show Statement where 
   		show (Tautology formula) = "taut " ++ show formula ++ " : " ++ show (is_tautology formula)
   		show (Satisfiable formula) = "sat " ++ show formula ++ " : " ++ show (is_satisfiable formula)

	-- Making the language
   	languageDef = emptyDef {
   								Token.identStart=letter
   							,   Token.identLetter = alphaNum
   							,   Token.reservedNames=[ "taut", "sat", "T", "F" ]
   							,   Token.reservedOpNames=[ "~", "/\\", "\\/", "=>" , "<=>" ]
   							,   Token.caseSensitive=False
   						   }


	lexer = Token.makeTokenParser languageDef
	identifier = Token.identifier lexer
	reserved = Token.reserved lexer
	reservedOp = Token.reservedOp lexer
	parens = Token.parens

	whiteSpace = Token.whiteSpace lexer

	whileParser :: Parser Statement
	whileParser = whiteSpace >> statement

	statement :: Parser Statement
	statement =     tautologyStatement
				<|> satisfiableStatement

	tautologyStatement = 
						do reserved "taut" 
						   formula <- formulaExpression
						   return $ Tautology formula

	satisfiableStatement = 
						do reserved "sat"
						   formula <- formulaExpression
						   return $ Satisfiable formula

	formulaExpression :: Parser Formula
	formulaExpression = buildExpressionParser operators terms
	
	-- The operators in our formula
	operators = [
					  [ Prefix (reservedOp "~" >> return (Not )) ]
					, [
						Infix (reservedOp "/\\" >> return (And )) AssocLeft 
					  , Infix (reservedOp "\\/" >> return (Or )) AssocLeft  
					  ]
					, [ Infix (reservedOp "=>" >> return (Impl )) AssocLeft ]
					, [ Infix (reservedOp "<=>" >> return (Eqv )) AssocLeft ]
				]


	-- terminal formula expressions
	terms = parens lexer formulaExpression
		<|> (reserved "T"  >> return FTrue )
		<|> (reserved "F" >> return FFalse)
		<|> (Atom <$> identifier)

	-- The function for parsing a String
	-- It returns a String no matter the outcome of the parsing
	parseString :: String -> String
	parseString str =
		case parse whileParser "" str of
			Left e  -> "Parsing error: " ++ show e
			Right r -> show r

   