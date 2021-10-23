main = do
	line <- getLine
	if line == "42"
		then return ()
	else do
		putStrLn line
		main
