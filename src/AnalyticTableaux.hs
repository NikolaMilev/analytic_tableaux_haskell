module AnalyticTableaux(is_tautology, is_satisfiable) where
	-- we allow only for these two functions to be visible outside the module: is_tautology and is_satisfiable
	-- that way, we present the api and only the api
	-- all the types are annotated
	import Formula
	import NNF 
	import qualified Data.Map.Strict as Map

	-- we put the encountered atoms in a map
	is_closed :: Map.Map [Char] Bool -> [Formula] -> Bool
	
	-- If we encounter the case where there are no more formulas to examine within the current branch,
	-- the branch is open (and therefore not closed)
	is_closed branch [] = False

	-- If we encounter conjunction, we add both formulas to the list of formulas 
	is_closed branch ((And f1 f2):xs) = is_closed branch (f1:f2:xs) 

	-- If we encounter disjunction, we determine if both branches are closed; if they are,
	-- then current branch is closed
	is_closed branch ((Or f1 f2):xs) = (is_closed branch (f1:xs)) && (is_closed branch (f2:xs))

	-- if we encounter a negation of an atom, we search for it in the branch argument (a [Char] Bool map)
	-- if we find it and it's set true in the map, the branch is closed
	-- if we find it and it's set false, we continue
	-- if we don't find it, we set it false and move on
	is_closed branch ((Not (Atom c)):xs) = case Map.lookup c branch of 
		                              Nothing -> is_closed (Map.insert c False branch) xs
		                              Just False -> is_closed branch xs
		                              Just True -> True

	-- if we encounter an atom, we search for it in the branch argument (a [Char] Bool map)
	-- if we find it and it's set false in the map, the branch is closed
	-- if we find it and it's set true, we continue
	-- if we don't find it, we set it true and move on
	is_closed branch ((Atom c):xs) = case Map.lookup c branch of 
		                              Nothing -> is_closed (Map.insert c True branch) xs
		                              Just True -> is_closed branch xs
		                              Just False -> True

	-- if we encounter a False literal within a branch, it is considered closed
	is_closed branch ((FFalse):xs) = True

	-- if we encounter a True literal within a branch, it is considered open
	is_closed branch ((FTrue):xs) = is_closed branch xs

	-- returns true if the given formula is a tautology
	is_tautology :: Formula -> Bool
	is_tautology formula = is_closed Map.empty [nnf $ Not formula]

	-- returns true if the given formula is satisfiable
	is_satisfiable :: Formula -> Bool
	is_satisfiable formula = not $ is_closed Map.empty [nnf formula]