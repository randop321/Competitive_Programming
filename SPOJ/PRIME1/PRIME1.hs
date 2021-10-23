import qualified Data.Array as V

checker :: Int -> Int -> Int
checker 1 i = 0
checker n i
    | i*i > n = 1
    | n `mod` i == 0 = 0
    | otherwise = checker n (i+1)

primes_list = [i | i <- [2..33000], (checker i  2) == 1] :: [Int]

primes = V.array (0, 3537) [(i, primes_list!!i) | i <- [0..3537]]

checker2 :: Int -> Int -> Int
checker2 1 i = 0
checker2 n i
    | i >= 3538 = 1
    | (primes V.! i)*(primes V.! i) > n = 1
    | n `mod` (primes V.! i) == 0 = 0
    | otherwise = checker2 n (i+1)


process 0 = return ()
process n = 
    do
        buff <- getLine
        let y = words buff
        let x = map (read :: String -> Int) y
        let l = x !! 0
        let r = x !! 1

        let res = [i | i <- [l..r], (checker2 i  0) == 1]
        mapM_ print res
        putStrLn ""
        process (n-1)

main = do
    t_ <- getLine
    let t = (read t_ :: Int)
    process t
    
    
