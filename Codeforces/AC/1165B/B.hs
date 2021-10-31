import Data.List

process :: Int -> [Int] -> Int
process _ [] = 0
process x (y:ys)
    | x<=y = 1 + (process (x+1) ys)
    | otherwise = process x ys

main = do
    t_ <- getLine
    let t = (read t_ :: Int)
    x_ <- getLine
    let x = map (read::String->Int) (words x_)
    let y = (sort x)
    print (process 1 y)
