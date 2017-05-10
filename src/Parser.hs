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
	
	-- za ovaj treba parsec a za parser treba megaparsec

	-- source
	-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

	-- better source: https://wiki.haskell.org/Parsing_a_simple_imperative_language
	
	data Statement =  Tautology Formula
					| Satisfiable Formula 
					deriving (Eq)
	
	evaluate :: Statement -> Bool
 	evaluate (Satisfiable formula) = is_satisfiable formula
   	evaluate (Tautology formula) = is_tautology formula

   	instance Show Statement where 
   		show (Tautology formula) = "taut " ++ show formula ++ " : " ++ show (is_tautology formula)
   		show (Satisfiable formula) = "sat " ++ show formula ++ " : " ++ show (is_satisfiable formula)

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
	
	operators = [
					  [ Prefix (reservedOp "~" >> return (Not )) ]
					, [
						Infix (reservedOp "/\\" >> return (And )) AssocLeft 
					  , Infix (reservedOp "\\/" >> return (Or )) AssocLeft  
					  ]
					, [ Infix (reservedOp "=>" >> return (Impl )) AssocLeft ]
					, [ Infix (reservedOp "<=>" >> return (Eqv )) AssocLeft ]
				]

	parseString :: String -> String
	parseString str =
		case parse whileParser "" str of
			Left e  -> error $ show e
			Right r -> show r

	-- ubaci nekako u terms:
	-- = (parens formulaExpression)
	terms = parens lexer formulaExpression
		<|> (reserved "T"  >> return FTrue )
		<|> (reserved "F" >> return FFalse)
		<|> (Atom <$> identifier)

   