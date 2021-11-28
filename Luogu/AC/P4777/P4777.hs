{-# Options_GHC -O2 #-}
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

exCRT :: (Integral a) => (a,a) -> (a,a) -> (a,a)
exCRT (a,b) (c,d) = (e,f)
    where
        (t1',t2',g) = extended_gcd b d
        t1 = t1' * (div (c-a) g)
        f = div (b*d) g
        e = mod ((mod (a + t1*b) f) + f) f

extendedChineseRemainder :: (Integral a) => [(a,a)] -> (a,a)
extendedChineseRemainder eqs = foldl1 (\a b -> exCRT a b) eqs

readInt = fst . fromJust . BS.readInt
readInteger = fst . fromJust . BS.readInteger
readIntegerList = map readInteger . BS.words
getInt = readInt <$> BS.getLine
getIntegerList = readIntegerList <$> BS.getLine


main = do
    n <- getInt
    d <- sequence $ replicate n getIntegerList
    let eqs = map (\[b,a] -> (a,b)) d
    print $ fst $ extendedChineseRemainder eqs
