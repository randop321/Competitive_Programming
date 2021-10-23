isPrime n = (n > 1) && null [i | i <- [2..floor(sqrt(fromIntegral n))], mod n i == 0]
primes_list = [i | i <- [2..33000], isPrime i]
isPrime2 n = (n>1) && null [i | i <- primes_list, i < n, mod n i == 0]

process 0 = return ()
process n = 
    do
        buff <- getLine
        let y = words buff
        let x = map (read :: String -> Int) y
        let l = x !! 0
        let r = x !! 1

        let res = [i | i <- [l..r], isPrime2 i]
        mapM_ print res
        putStrLn ""
        process (n-1)

main = do
    t_ <- getLine
    let t = (read t_ :: Int)
    process t
    
    
