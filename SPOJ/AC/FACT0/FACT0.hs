{-

-}


{-# Options_GHC -O2 #-}
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import System.Random
import Data.List

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

readInteger = fst . fromJust . BS.readInteger
readIntegerList = map readInteger . BS.words
getInteger = readInteger <$> BS.getLine
getIntegerList = readIntegerList <$> BS.getLine


getStrLn = BS.unpack <$> BS.getLine

putListLn = \x -> putStrLn $ tail $ x >>=  (' ':)



fastpow :: (Integral a) => a -> a -> a -> a
fastpow a 0 b = 1
fastpow a n b
    | odd n = mod (a*res) b
    | otherwise = res
    where res = fastpow (mod (a*a) b) (div n 2) b

-- Miller Rabin Test
--  find2kb n -> (k,b) s.t. 2^k*b = n
find2kb :: Integral a => a -> (Int,a)
find2kb n
    | mod n 2 == 1 = (0,n)
    | otherwise = (k+1,b)
        where (k,b) = find2kb (div n 2)

millerRabinSeqTest :: (Integral a) => a -> Int -> a -> Bool
millerRabinSeqTest v k n
    | k < 0 = False
    | otherwise = (v == n-1) || (millerRabinSeqTest (mod (v*v) n) (k-1) n)

millerRabinTest :: (Integral a) => a -> a -> Bool
millerRabinTest n a
    | a<1 || a>n-1 = 
        error $ "Failed to Miller-Rabin Test: out of range"
    | n == 2 = True
    | n < 2 = False
    | mod n 2 == 0 = False
    | otherwise = (init_val == 1) || (millerRabinSeqTest init_val k n)
    where
        (k,b) = find2kb (n-1)
        init_val = fastpow a b n

-- isPrime n: return True if n is prime
isPrime :: (Integral a) => a -> Int -> Bool
isPrime n seed
    | n <= 1 = False
    | n == 2 = True
    | mod n 2 == 0 = False
    | otherwise = and $ map (millerRabinTest n) randomlist
    where
        init_val = (1, mkStdGen seed)
        randomlist = map (\x -> (mod (fromIntegral $ fst x) (n-1))+1) $ scanl (\a x -> next $ snd a) init_val [1..30]
        
        
        
-- Pollard's Rho Algorithm
-- assuming n is not prime
pollardsRhoSeq :: (Integral a) => a -> a -> a -> a -> a
pollardsRhoSeq n x y k
    | g == 1 = pollardsRhoSeq n xx yy k
    | g == n = n
    | otherwise = g
    where
        g = gcd (abs (xx-yy))  n
        xx = mod (x*x+k) n
        y1 = mod (y*y+k) n
        yy = mod (y1*y1+k) n

pollardsRho :: (Integral a) => a -> StdGen -> a
pollardsRho n stdgen
    | (mod n 2) == 0 = 2
    | g == n = pollardsRho n c
    | otherwise = g
    where
        g = pollardsRhoSeq n m m k
        a = next stdgen
        b = next $ snd a
        c = snd b
        m = fromIntegral $ fst a
        k = fromIntegral $ fst b
        
primeFactoring :: (Integral a) => a -> Int -> [a]
primeFactoring n seed
    | n < 0 = -1 : (primeFactoring (-n) seed)
    | n <=1 = []
    | (isPrime n seed) == True = [n]
    | otherwise = (primeFactoring a seed) ++ (primeFactoring (div n a) seed)
    where
        a = pollardsRho n $ mkStdGen seed
        


process 0 = return()
process n = 
    do
        let a = primeFactoring (fromIntegral n) 1231
        let b = sort $ nub a
        let c = [ length [y | y <- a, y==xx] | xx <- b]
        let d = [show x ++ "^" ++ show y | (x,y) <- zip b c]
        putListLn d
        n <- getInt
        process n
        
main = do
    n <- getInt
    process n
