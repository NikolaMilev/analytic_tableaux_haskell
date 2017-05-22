{-|
	Module      : Parser
	Description : The parser for the Formula data type.
	Copyright   : (c) Nikola Milev, 2017.
	License     : None
	Maintainer  : nikola.n.milev@gmail.com
	Stability   : Stable
	Portability : Any platform that supports cabal, except for the .cabal file itself.

	Here, we parse the String input and return a Formula instance. We do a little more,
	having declared the Statement data type and parsing the statement itself. It represents
	the question if the formula is a tautology/satisfiable.
	The parser is implemented using the Parsec library. Most of the functions are just Parsec library standard functions.
	For more info, see: https://wiki.haskell.org/Parsing_a_simple_imperative_language
-}
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
	

	
	
	{-|
		The data structure representing one statement within our interpreter.
		The statement is keyword 'taut' or 'sat' followed by a formula .
	-}
	data Statement =  Tautology Formula
					| Satisfiable Formula 
					deriving (Eq)
	
	{-|
		The semantical evaluation of a Statement instance.
		The declaration and the definition are more than clear, I think. 
	-}
	evaluate :: Statement -> Bool
 	evaluate (Satisfiable formula) = is_satisfiable formula
   	evaluate (Tautology formula) = is_tautology formula

   	{-|
		The show function for the Statement. Also contains the evaluation for the 
		easiest process of examining if the formula is a tautology / satisfiable.
	-}
   	instance Show Statement where 
   		show (Tautology formula) = "taut " ++ show formula ++ " : " ++ show (is_tautology formula)
   		show (Satisfiable formula) = "sat " ++ show formula ++ " : " ++ show (is_satisfiable formula)

	{-|
		The definition of the language lexemes.
	-}
   	languageDef = emptyDef {
   								Token.identStart=letter
   							,   Token.identLetter = alphaNum
   							,   Token.reservedNames=[ "taut", "sat", "T", "F" ]
   							,   Token.reservedOpNames=[ "~", "/\\", "\\/", "=>" , "<=>" ]
   							,   Token.caseSensitive=False
   						   }


	{-|
		The lexer.
	-}
	lexer = Token.makeTokenParser languageDef
	
	{-|
		The identifier parser.
	-}
	identifier = Token.identifier lexer
	
	{-|
		The identifier parser.
	-}
	reserved = Token.reserved lexer
	
	{-|
		The operator parser.
	-}
	reservedOp = Token.reservedOp lexer

	{-|
		The parentheses parser.
	-}
	parens = Token.parens

	{-|
		The whitespace consumer.
	-}
	whiteSpace = Token.whiteSpace lexer


	{-|
		The main parser.
	-}
	whileParser :: Parser Statement
	whileParser = whiteSpace >> statement
	
	{-|
		The statement parser.
	-}
	statement :: Parser Statement
	statement =     tautologyStatement
				<|> satisfiableStatement

	{-|
		The parser for the tautology statement.
	-}
	tautologyStatement = 
						do reserved "taut" 
						   formula <- formulaExpression
						   return $ Tautology formula
	{-|
		The parser for the satisfiable statement.
	-}
	satisfiableStatement = 
						do reserved "sat"
						   formula <- formulaExpression
						   return $ Satisfiable formula

	{-|
		The parser the formula.
	-}
	formulaExpression :: Parser Formula
	formulaExpression = buildExpressionParser operators terms
	
	{-|
		The operators that can be in a formula. The expansion of this table is
		inevitable if we expand the Formula data type.
	-}
	operators = [
					  [ Prefix (reservedOp "~" >> return (Not )) ]
					, [
						Infix (reservedOp "/\\" >> return (And )) AssocLeft 
					  , Infix (reservedOp "\\/" >> return (Or )) AssocLeft  
					  ]
					, [ Infix (reservedOp "=>" >> return (Impl )) AssocLeft ]
					, [ Infix (reservedOp "<=>" >> return (Eqv )) AssocLeft ]
				]


	{-|
		The terminal expressions are as follows:
			- True constant
			- False constant
			- Atom (variable)
	-}
	terms = parens lexer formulaExpression
		<|> (reserved "T"  >> return FTrue )
		<|> (reserved "F" >> return FFalse)
		<|> (Atom <$> identifier)

	{-|
		The String parser; Given a String object, it parses a Statement object and
		prints it. 
	-}
	parseString :: String -> String
	parseString str =
		case parse whileParser "" str of
			Left e  -> "Parsing error: " ++ show e
			Right r -> show r

   