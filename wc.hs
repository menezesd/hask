main = interact wc
	where wc input = show (length (lines input)) ++ " " ++ show (length (words input)) ++ " " ++ show (length input) ++ "\n"
