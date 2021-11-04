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

calcEditDist :: [Int] -> [Int] -> Int
calcEditDist [] [] = 0
calcEditDist (x:xs) (y:ys) = (calcEditDist xs ys) + (abs (x-y))

elemIndexwithList :: [Char] -> Char -> Maybe Int
elemIndexwithList a b = elemIndex b a

process 0 = return ()
process t = do
    s <- getLine
    p <- getLine
    let _:a = p
    let b = init p
    let idx0 = map (fromJust . elemIndexwithList s) a
    let idx1 = map (fromJust . elemIndexwithList s) b
    print (calcEditDist idx0 idx1)
    process (t-1)
    
main = do
    t <- getInt
    process t
