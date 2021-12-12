import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Maybe
import Data.List

primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,57]

find2kb :: Integral a => a -> (Int,a)
find2kb n
    | mod n 2 == 1 = (0,n)
    | otherwise = (k+1,b)
        where (k,b) = find2kb (div n 2)
        
--- fastpow a n b = a^n % b
fastpow :: (Integral a) => a -> a -> a -> a
fastpow a 0 b = 1
fastpow a n b
    | odd n = mod (a*res) b
    | otherwise = res
    where res = fastpow (mod (a*a) b) (div n 2) b

sqrtRootModP :: (Integral a, Show a) => a -> a -> [String]
sqrtRootModP n p 
    | n' == 0 = ["0"]
    | (fastpow n' (div (p-1) 2)  p) == (p-1) = ["Hola!"]
    | otherwise = map show $ nub $ sort $ [res, p-res]
    where
        n' = mod n p
        res = tonelliShanks n' p

tonelliShanksSeq :: (Integral a) => a -> a -> a -> Int -> Int -> a -> a -> a
tonelliShanksSeq r n t s m b p
    | t == 1 = r
    | (fastpow t (fromInteger $ shift 1 (m-1)) p) == 1 = tonelliShanksSeq r n t s (m-1) b p
    | otherwise = tonelliShanksSeq (mod (r*c) p) n (mod ((mod (t*c) p)*c) p) s (m-1) b p
    where
        c = fastpow b (fromInteger $ shift 1 (s-1-m)) p

tonelliShanks :: (Integral a) => a -> a -> a
tonelliShanks n p = tonelliShanksSeq r n t s (s-1) b p
    where
        (s,q) = find2kb (p-1)
        z = fromInteger $ head [i | i <- primes, (fastpow (fromInteger i)  (div (p-1) 2)  p) == p-1]
        r = fastpow n (div (q+1) 2) p
        t = fastpow n q p
        b = fastpow z q p

readInt = fst . fromJust . BS.readInt
readInteger = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine


process 0 = return ()
process t = do
    [n,p] <- getIntList
    let res = sqrtRootModP n p
    putStrLn $ tail $ res >>= (' ':)
    process (t-1)
    
    
    
main = do
    t <- getInt
    process t
