module AnalyticTableaux where
	-- http://dev.stephendiehl.com/fun/002_parsers.html
	-- za parsiranje ^
	import Formula
	import NNF 
	import qualified Data.Map.Strict as Map

	-- we put the encountered atoms in a map
	is_closed :: Map.Map [Char] Bool -> [Formula] -> Bool
	
	-- If we encounter the case where there are no more formulas to examine within the current branch,
	-- the branch is open
	is_closed branch [] = False

	-- If we encounter conjunction, we add both formulas to the list of formulas 
	is_closed branch ((And f1 f2):xs) = is_closed branch (f1:f2:xs) 

	-- If we encounter disjunction, we determine if both branches are closed; if they are,
	-- then current branch is closed
	is_closed branch ((Or f1 f2):xs) = (is_closed branch (f1:xs)) && (is_closed branch (f2:xs))

	is_closed branch ((Not (Atom c)):xs) = case Map.lookup c branch of 
		                              Nothing -> is_closed (Map.insert c False branch) xs
		                              Just False -> is_closed branch xs
		                              Just True -> True


	is_closed branch ((Atom c):xs) = case Map.lookup c branch of 
		                              Nothing -> is_closed (Map.insert c True branch) xs
		                              Just True -> is_closed branch xs
		                              Just False -> True

	is_closed branch ((FFalse):xs) = True
	is_closed branch ((FTrue):xs) = is_closed branch xs

	is_tautology :: Formula -> Bool
	is_tautology formula = is_closed Map.empty [nnf $ Not formula]

	is_satisfiable :: Formula -> Bool
	is_satisfiable formula = not $ is_closed Map.empty [nnf formula]