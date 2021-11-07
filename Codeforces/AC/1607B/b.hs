{-
    (pos, delta)
    (0,1) -> (1,0) -> (1,1) -> (0,0) -> (0,1) ...
    (1,1) -> (0,0) -> (0,1) -> (1,0) -> (1,1) ...
    which mean if the init position is even, and the action list is (-1 +2 +3 -4), (-5 +6 +7 -8), ...
    and if the init position is odd, then the action list is (+1 -2 -3 +4), (+5 -6 -7 +8), ...
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

process 0 = return ()
process t = do
    [a,b] <- getIntList
    let c = mod b 4
    let tmp = if c==0 then 0 else if c==1 then b else if c==2 then -1 else -b-1
    let res = if odd a then tmp else -tmp
    print (a+res)
    process (t-1)
    
main = do
    t <- getInt
    process t
