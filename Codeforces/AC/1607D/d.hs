{-
    greedy algorithms:
    - group and sort ai with its color => two lists r, b
    - consider each number i in the permutation 1..n in increased order
      if the list b is not empty
          if the minimum element of b is less than i, then no transformed solution, because we cannot increase the value of any element in b
          otherwise we use and delete it from b and consider (i+1)
              Why we don't use other elements in b? Because we consider i in increased order, the minimum element of b is the worst case in the future.
              Why we don't use elements in r? Because for each element r' in r, if r' < i which means r'will always be valid in the future, otherwise we cannot use r' currently.
      else if the list r is not empty
          if the minimum element of r is greater than i, then no solution of course
          otherwise we use and delete it from r and consider (i+1)
      otherwise it means we find a solution.
-}

{-# Options_GHC -O2 #-}
import Data.Char
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.List

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

mysplit :: [Int] -> String -> Char -> [Int]
mysplit [] _ _ = []
mysplit (x:x1) (s:s1) key
    | s == key = x: (mysplit x1 s1 key)
    | otherwise = mysplit x1 s1 key

checker :: Int -> [Int] -> [Int] -> Bool
checker _ [] [] = True
checker n r (b:b1)
    | b < n = False
    | otherwise = checker (n+1) r b1
checker n (r:r1) []
    | r > n = False
    | otherwise = checker (n+1) r1 []

process 0 = return ()
process t = do
    n <- getInt
    vals <- getIntList
    keys <- getLine
    let r = sort $ mysplit vals keys 'R'
    let b = sort $ mysplit vals keys 'B'
    putStrLn $ if (checker 1 r b) then "YES" else "NO"
    process (t-1)
    
main = do
    t <- getInt
    process t
