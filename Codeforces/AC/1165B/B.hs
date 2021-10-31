{-# Options_GHC -O2 #-}

import Data.List
import Data.Char
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
  

process :: Integer -> [Integer] -> Integer
process _ [] = 0
process x (y:ys)
    | x<=y = 1 + (process (x+1) ys)
    | otherwise = process x ys

main = do
    t_ <- getLine
    let t = (read t_ :: Int)
    
    x <- getIntList
    let y = (sort x)
    print (process 1 y)
