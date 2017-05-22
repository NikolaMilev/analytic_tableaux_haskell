{-|
	Module      : Formula
	Description : The abstract tree that describes the formulae.
	Copyright   : (c) Nikola Milev, 2017.
	License     : None
	Maintainer  : nikola.n.milev@gmail.com
	Stability   : Stable
	Portability : Any platform that supports cabal, except for the .cabal file itself.

	In this module, there is the definition of the Formula data type, represented
	as an abstract syntax tree.
	Also, there is the show function implemented for the formula. It is not perfect (there
	is a lot of parentheses) but it works. You are welcome to improve the show function.
-}
module Formula where
	{-|
		The definition of the Formula data type, represented as an abstract syntax tree.
		The possible operators are conjunction, disjunction, implication, equivalence and
		negation. The possible terminating formulae are atoms (variables) and constants.
	-}
	data Formula = And Formula Formula | 
	               Or Formula Formula | 
	               Impl Formula Formula |
	               Eqv Formula Formula |
	               Not Formula | 
	               Atom [Char] | 
	               FTrue |
	               FFalse
	               deriving (Eq)


	{-|
		The show function for the Formula data type.
	-}
	instance Show Formula where
		show (And f1 f2) = "(" ++ show f1 ++ " /\\ " ++ show f2 ++ ")"
		show (Or f1 f2) = "(" ++ show f1 ++ " \\/ " ++ show f2 ++ ")"
		show (Impl f1 f2) = "(" ++ show f1 ++ " => " ++ show f2 ++ ")"
		show (Eqv f1 f2) = "(" ++ show f1 ++ " <=> " ++ show f2 ++ ")"
		show (Not f) = "( ~" ++ show f ++ ")"
		show (Atom c) = c
		show FTrue = "T"
		show FFalse = "F"

    