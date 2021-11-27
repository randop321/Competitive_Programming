{-# Options_GHC -O2 #-}
import System.Random
import Data.Char
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.List
import Text.Printf

extended_gcd :: (Integral a) => a -> a -> (a, a, a)
extended_gcd a 0 = (1,0,a)
extended_gcd a b = (_y, _x -  (div a b) *_y, gcd)
    where (_x, _y, gcd) = extended_gcd b (mod a b)

extract1of3 :: (a,a,a) -> a
extract1of3 (a,_,_) = a

chineseRemainder :: (Integral a) => [a] -> [a] -> a
chineseRemainder a b = mod ((mod (sum [bi*si | (bi,si) <- zip b s]) m)+m) m
    where
        m = product a
        n = [div m ai | ai <- a]
        k = [extract1of3 $ extended_gcd (mod ni ai) ai | (ni,ai) <- zip n a]
        s = [mod (ni*ki) m | (ni,ki) <- zip n k]

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine


main = do
    n <- getInt
    d <- sequence $ replicate n getIntList
    let a = map head d
    let b = map (!!1) d
    print $ chineseRemainder a b
