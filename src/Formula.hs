-- The abstract tree that describes the formula
module Formula where
	data Formula = And Formula Formula | 
	               Or Formula Formula | 
	               Impl Formula Formula |
	               Eqv Formula Formula |
	               Not Formula | 
	               Atom [Char] | 
	               FTrue |
	               FFalse
	               deriving (Eq)


	-- printing of the formula tree
	instance Show Formula where
		show (And f1 f2) = "(" ++ show f1 ++ " /\\ " ++ show f2 ++ ")"
		show (Or f1 f2) = "(" ++ show f1 ++ " \\/ " ++ show f2 ++ ")"
		show (Impl f1 f2) = "(" ++ show f1 ++ " => " ++ show f2 ++ ")"
		show (Eqv f1 f2) = "(" ++ show f1 ++ " <=> " ++ show f2 ++ ")"
		show (Not f) = "( ~" ++ show f ++ ")"
		show (Atom c) = c
		show FTrue = "T"
		show FFalse = "F"

    