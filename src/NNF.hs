{-|
	Module      : NNF
	Description : The metods for examining the analytic tableaux for formulae.
	Copyright   : (c) Nikola Milev, 2017.
	License     : None
	Maintainer  : nikola.n.milev@gmail.com
	Stability   : Stable
	Portability : Any platform that supports cabal, except for the .cabal file itself.

	In this module, there are functions that transform the formula into the NNF (Negation
	Normal Form).
	For more info, see: https://en.wikipedia.org/wiki/Negation_normal_form

	We allow only for the nnf function to be visible outside the module;
	that way, we present the api and only the api.
	All the types are correctly annotated.
-}
module NNF(nnf) where
	import Formula

	{-| 
		The removal of the implication and the equivalence symbols;
    	This function should be called on the abstract formula tree before
    	any others, as they rely on the fact that the Formula does not
    	contain those.
    -}
	remove_eq_impl :: Formula -> Formula
	remove_eq_impl (And f1 f2) = And (remove_eq_impl f1) (remove_eq_impl f2)
	remove_eq_impl (Or f1 f2) = Or (remove_eq_impl f1) (remove_eq_impl f2)
	remove_eq_impl (Eqv f1 f2) = Or (And r_f1 r_f2) (And (Not r_f1) (Not r_f2))
		                        where r_f1 = remove_eq_impl f1
		                              r_f2 = remove_eq_impl f2
	remove_eq_impl (Impl f1 f2) = Or (Not r_f1) f2
                                  where r_f1 = remove_eq_impl f1
                                        r_f2 = remove_eq_impl f2
	remove_eq_impl (Not f) = Not (remove_eq_impl f)

	{-| 
		In case the formula definition needs to be expanded, this is here so that
		the code still compiles and runs.
    -}
	remove_eq_impl formula = formula
	
	{-| 
		Translates the formula to NNF. Not implemented for equivalence
		and implication. See the docs for remove_eq_impl function.
		Also, the constants are not  removed by this function, it is the job
		of the simplify function.
    -}
	to_nnf :: Formula -> Formula
	to_nnf (Not (And f1 f2)) = Or r_f1 r_f2
	                          where r_f1 = to_nnf (Not f1)
	                                r_f2 = to_nnf (Not f2)
	to_nnf (Not (Or f1 f2)) = And r_f1 r_f2
	                          where r_f1 = to_nnf (Not f1)
	                                r_f2 = to_nnf (Not f2)
	to_nnf (Not (Not f)) = r_f
	                       where r_f = to_nnf f  
	to_nnf (And f1 f2) = And (to_nnf f1) (to_nnf f2) 
	to_nnf (Or f1 f2) = Or (to_nnf f1) (to_nnf f2) 
	to_nnf (Not f) = Not (to_nnf f) 
	
	{-| 
		In case the formula definition needs to be expanded, this is here so that
		the code still compiles and runs.
    -}
	to_nnf formula = formula 

	{-| 
		Translates the formula to a form without the constants. Not implemented
		for equivalence and implication.
    -}
	simplify :: Formula -> Formula
	simplify (Or _ FTrue) = FTrue
	simplify (Or FTrue _) = FTrue
	simplify (Or FFalse f) = simplify f
	simplify (Or f FFalse) = simplify f
	simplify (Or f1 f2) = case (simplify f1, simplify f2) of
                               (FTrue, _) -> FTrue
                               (_, FTrue) -> FTrue
                               (FFalse, formula) -> formula
                               (formula, FFalse) -> formula
                               (rf1, rf2) -> Or rf1 rf2

	simplify (And f FTrue) = simplify f
	simplify (And FTrue f) = simplify f
	simplify (And FFalse _) = FFalse
	simplify (And _ FFalse) = FFalse
	simplify (And f1 f2) = case (simplify f1, simplify f2) of
                               (FFalse, _) -> FFalse
                               (_, FFalse) -> FFalse
                               (FTrue, formula) -> formula
                               (formula, FTrue) -> formula
                               (rf1, rf2) -> And rf1 rf2
	simplify (Not FFalse) = FTrue
	simplify (Not FTrue) = FFalse
	simplify (Not f) = case (simplify f) of
		                     FTrue -> FFalse
		                     FFalse -> FTrue
		                     formula -> Not formula
	simplify formula = formula

	{-| 
		Converts the formula to NNF. Makes no assumptions about the structure
		of the formula. After applying this function to a formula, the only possible nodes
		are And, Or, Not and Atom.
    -}
	nnf :: Formula -> Formula
	nnf formula = simplify $ to_nnf $ remove_eq_impl formula
