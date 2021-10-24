procOprs :: String -> String -> String
procOprs [] cond = []
procOprs (x:xs) cond = 
    if elem x cond 
    then 
        if x == '(' then [] else x:(procOprs xs cond )
    else []

popStack :: String -> String -> String
popStack [] cond = []
popStack (x:xs) cond = 
    if elem x cond 
    then 
        if x == '(' then xs else popStack xs cond 
    else x:xs



getRPN :: String -> String -> String
getRPN [] oprs = reverse [x | x <- oprs, x/=')', x/='(']
getRPN ('(':xs) oprs = getRPN xs ('(':oprs)
getRPN (x:xs) oprs = 
    if elem x ['a'..'z'] then x:(getRPN xs oprs)
    else procOprs oprs (cond x) ++ getRPN xs ((if x == ')' then "" else [x]) ++ popStack oprs (cond x))
        where   cond '^' = "^"
                cond '/' = "^/"
                cond '*' = "^/*"
                cond '-' = "^/*-"
                cond '+' = "^/*-+"
                cond ')' = "^/*-+("


    
process 0 = return ()
process n = 
    do
        buff <- getLine
        putStrLn (getRPN buff "")
        process (n-1)

main = do
    t_ <- getLine
    let t = (read t_ :: Int)
    process t
    
    
