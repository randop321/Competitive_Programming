import System.Random

data Tree = EmptyTree | Node Int Int Int Tree Tree deriving (Show)

create :: Int -> Int -> Tree
create value rand = Node value 1 rand EmptyTree EmptyTree

update :: Tree -> Tree
update EmptyTree = EmptyTree
update (Node value cnt rand EmptyTree EmptyTree) = Node value 1 rand EmptyTree EmptyTree
update (Node value cnt rand (Node v1 c1 r1 left1 right1) EmptyTree) = Node value (c1+1) rand (Node v1 c1 r1 left1 right1) EmptyTree
update (Node value cnt rand EmptyTree (Node v2 c2 r2 left2 right2)) = Node value (c2+1) rand EmptyTree (Node v2 c2 r2 left2 right2)
update (Node value cnt rand (Node v1 c1 r1 left1 right1) (Node v2 c2 r2 left2 right2)) = Node value (c1+c2+1) rand (Node v1 c1 r1 left1 right1) (Node v2 c2 r2 left2 right2)


insert :: (Int, Int) -> Tree -> Tree
insert (key, rkey) EmptyTree = create key rkey
insert (key, rkey) (Node v c r left right)
    | key < v = update (Node v c r (insert (key, rkey) left) right)
    | key > v = update (Node v c r left (insert (key, rkey) right))


getTreeSize :: Tree -> Int
getTreeSize EmptyTree = 0
getTreeSize (Node v c r left right) = c

find :: Int -> Tree -> Int
find key EmptyTree = -1000000000
find key (Node v c r left  right)
    | key == v = (getTreeSize left) + 1
    | key < v = find key left
    | key > v = ((getTreeSize left) + 1) + (find key right)


process :: (Tree,[Int]) -> (String, (Int, Int)) -> (Tree, [Int])
process (t, res) ("1", (v, r)) = (insert ((-v), r) t, res)
process (t, res) ("2", (v, r)) = (t, (find (-v) t):res)

main = do
    t_ <- getLine
    let t = (read t_ :: Int)
        
    rs <- sequence $ replicate t $ randomRIO(1, 100000000::Int)
    
    a_ <- sequence $ replicate t $ getLine
    let a = [words i | i <- a_]
    let b0 = [head i | i <- a]
    let b = [last i | i <- a]
    let c = map (read::String->Int) b
    let d = zip c rs
    let e = zip b0 d
    
    let (_,f) = foldl process (EmptyTree,[]) e
    let g = reverse f
    let h = [if i <=0 then "Data tidak ada" else show i| i <- g]
    mapM_ putStrLn h
