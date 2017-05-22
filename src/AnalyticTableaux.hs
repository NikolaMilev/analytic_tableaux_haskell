{-|
	Module      : AnalyticTableaux
	Description : The metods for examining the analytic tableaux for formulae.
	Copyright   : (c) Nikola Milev, 2017.
	License     : None
	Maintainer  : nikola.n.milev@gmail.com
	Stability   : Stable
	Portability : Any platform that supports cabal, except for the .cabal file itself.

	In this module, there is a function for determining if a analytic tableaux for lists of formulae are
	closed and functions to determine if a formula is satisfiable or a tautology.
	We allow only for these two functions to be visible outside the module: is_tautology and is_satisfiable;
	that way, we present the api and only the api.
	The functions require the formulae to be in the NNF. See "NNF" module for more info.
	All the types are correctly annotated.
	For more info, see: https://en.wikipedia.org/wiki/Method_of_analytic_tableaux
-}
module AnalyticTableaux(is_tautology, is_satisfiable) where
	import Formula
	import NNF 
	import qualified Data.Map.Strict as Map

	{-|
		This function checks if the analytic tableaux for the given list of formulas are closed. We think
		of the list as a conjunction of the given formulas. If the tableau for any formula in the list
		is closed, then the tableau for the list as a whole is closed.
		The function takes two parameters and returns True if the tableau is closed, False otherwise.
		The firs parameter is a Map from String ([Char]) to Bool that denotes any found atomic formulae other
		than the True/False constants, id est the variables. If a variable is found within a sequence, then the
		pair (Variable, True) is inserted into the map, (Variable, False) if a negation of a variable is found.
		If, at any moment, both (Variable, True) and (Variable, False) are to be in the map, the current branch is
		closed and the function returns true.
		The second parameters is a list of the formulae for which we examine the tableaux.

		Here, we use the Haskell immutable data / reference system; insertion into the map returns a new reference. Thus we do not
		need to remove an item from the map if we have the old reference. Of course, the old structure is kept inside the
		new structure but it is not of much interest for this case.
	-}
	is_closed :: Map.Map [Char] Bool -> [Formula] -> Bool
	
	{-| 
		If we encounter the case where there are no more formulas to examine within the current branch,
	 	the branch is open (and therefore not closed) 
	 -}
	is_closed branch [] = False

	{-|
		If we encounter conjunction, we add both formulas to the list of formulas 
	-}
	is_closed branch ((And f1 f2):xs) = is_closed branch (f1:f2:xs) 

	{-| 
		If we encounter disjunction, we determine if both branches are closed; if they are,
		then current branch is closed
	-}
	is_closed branch ((Or f1 f2):xs) = (is_closed branch (f1:xs)) && (is_closed branch (f2:xs))

	{-| 
		If we encounter a negation of an atom, we search for it in the branch argument (a [Char] Bool map) ;
		if we find it and it's set true in the map, the branch is closed ;
		if we find it and it's set false, we continue ;
		if we don't find it, we set it false and move on.
	-}
	is_closed branch ((Not (Atom c)):xs) = case Map.lookup c branch of 
		                              Nothing -> is_closed (Map.insert c False branch) xs
		                              Just False -> is_closed branch xs
		                              Just True -> True

	{-|
		If we encounter an atom, we search for it in the branch argument (a [Char] Bool map) ;
		if we find it and it's set false in the map, the branch is closed ;
		if we find it and it's set true, we continue ;
		if we don't find it, we set it true and move on.
	-}
	is_closed branch ((Atom c):xs) = case Map.lookup c branch of 
		                              Nothing -> is_closed (Map.insert c True branch) xs
		                              Just True -> is_closed branch xs
		                              Just False -> True

	{-|
		If we encounter a False literal within a branch, it is considered closed. (A /\ False) = False
	-}
	is_closed branch ((FFalse):xs) = True

	{-|
		If we encounter a True literal within a branch, we move on. (A /\ True) = A
	-}
	is_closed branch ((FTrue):xs) = is_closed branch xs

	{-|
		This function checks if a formula is a tautology (always true). This is true
		if and only if the tableau is closed for the negation of the given formula.
		It takes one parameter, the formula, and returns True if it is a tautology and
		False otherwise.
	-}
	is_tautology :: Formula -> Bool
	is_tautology formula = is_closed Map.empty [nnf $ Not formula]

	{-|
		This function checks if a formula is satisfiable (if it can be true). This is true
		if and only if the tableau is not closed for the given formula.
		It takes one parameter, the formula, and returns True if it is satisfiable and
		False otherwise.
	-}
	is_satisfiable :: Formula -> Bool
	is_satisfiable formula = not $ is_closed Map.empty [nnf formula]