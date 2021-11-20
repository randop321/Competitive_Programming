{-
    Be careful with input data! Each line ends with \r\n.
    If I use getLine, whatever how to deal with it, I always get RE with code 1. I don't know why.
    Instead of that, I use BS.getLine and get AC.
    I have tried several approaches, such as takeWhile (not . isSpace) . getLine, and get RE too.
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

move :: [Int] -> Char -> [Int]
move [x,y] c = case c of
    'L' -> [x,y-1]
    'R' -> [x,y+1]
    'U' -> [x-1,y]
    'D' -> [x+1,y]
    _ -> [x,y]

process 0 = return ()
process t = do
    [n,m] <- getIntList
    --cmds <- getLine
    cmds <- takeWhile (not . isSpace) . BS.unpack <$> BS.getLine
    let delta = scanl (\p c -> (move p c)) [0,0] cmds
    let bound = scanl (\[x0,x1,y0,y1] [x,y] -> [min x0 x, max x1 x, min y0 y, max y1 y]) [0,0,0,0] delta
    let xxx = [[x1-x0+1, y1-y0+1] | [x0,x1,y0,y1] <- bound]
    let yyy = filter (\x -> ((x!!0)<=n) && ((x!!1)<=m) ) xxx
    let [x0,x1,y0,y1] = bound !! ((length yyy) -1)
    putStrLn $ tail $ [(-x0+1), (-y0+1)] >>= (' ':) . show


    process (t-1)
    
main = do
    t <- getInt
    process t
