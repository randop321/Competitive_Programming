--- Treap with only insert and find operators
--- It is very hard to pass the problem in the time constraint.
--- In order to same time, I combine the three vars value, cnt (the number of nodes in the tree), rand (a random number used in treap) into one Int as v = value * (2**36) + cnt * (2**18) + rand.
--- I also use ByteString to get input data, which is about 0.2s~0.3s time save compared with (read. getLine) on my computer.


{-# Options_GHC -O2 #-}
import System.Random
import Data.Char
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.Bits

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine


data Tree = EmptyTree | Node Int Tree Tree deriving (Show)

create :: Int -> Tree
{-# INLINE create #-}
create value = Node (value+1) EmptyTree EmptyTree



getTreeSize :: Tree -> Int
{-# INLINE getTreeSize #-}
getTreeSize EmptyTree = 0
getTreeSize (Node v left right) = ((.&.) v 262143)


rotateLeft :: Tree -> Tree
rotateLeft (Node v (Node v1 left1 right1) right)
    | ((.&.) v 68719476735) <= ((.&.) v1 68719476735) = Node v (Node v1 left1 right1) right
    | otherwise = Node (v1+1+(getTreeSize right)) left1 (Node (v-1-(getTreeSize left1)) right1 right)


rotateRight :: Tree -> Tree
rotateRight (Node v left (Node v1 left1 right1))
    | ((.&.) v 68719476735) <= ((.&.) v1 68719476735) = Node v left (Node v1 left1 right1)
    | otherwise = Node (v1+1+(getTreeSize left)) (Node (v-1-(getTreeSize right1)) left left1) right1
    
    
insert :: Int -> Tree -> Tree
insert key EmptyTree = create key
insert key (Node v left right)
    | key <= v = rotateLeft (Node (v+1) (insert key left) right)
    | key > v = rotateRight (Node (v+1) left (insert key right))




find :: Int -> Tree -> Int
find key EmptyTree = -1000000000
find key (Node v left right)
    | (shift key (-36)) == (shift v (-36)) = (getTreeSize left) + 1
    | key < v = find key left
    | otherwise = ((getTreeSize left) + 1) + (find key right)


process :: (Tree,[Int]) -> (Int, Int) -> (Tree, [Int])
process (t, res) (1, v) = (insert v t, res)
process (t, res) (2, v) = (t, (find v t):res)



main = do
    t_ <- getLine
    let t = (read t_ :: Int)
        
    rs <- sequence $ replicate t $ randomRIO(0, 262143::Int)
    --let rs = genRand t
    
    a <- sequence $ replicate t $ getIntList
    let b0 = [head i | i <- a]
    let b = [last i | i <- a]
    let d = zip b rs
    let d0 = [ x*68719476736+y*262144 | (x,y) <- d]
    let e = zip b0 d0
    
    let (_,f) = foldl process (EmptyTree,[]) e
    let g = reverse f
    let h = [if i <=0 then "Data tidak ada" else show i| i <- g]
    mapM_ putStrLn h
