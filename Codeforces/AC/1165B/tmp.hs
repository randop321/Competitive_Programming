--prewritten code: https://github.com/antma/algo
{-# LANGUAGE BangPatterns, FlexibleContexts, Safe #-}
{-# Options_GHC -O2 #-}
import Data.ByteString.Builder
import Data.Monoid
import System.IO
import qualified Data.ByteString.Char8 as C
import Data.Time (diffTimeToPicoseconds, utctDayTime, getCurrentTime)
import Data.Char
import qualified Data.Array.Unboxed as UA
import Control.Monad
import Control.Monad.ST
import Data.Array.ST.Safe

getSeed = return . fromInteger . (`mod` 999983) . diffTimeToPicoseconds . utctDayTime =<< getCurrentTime :: IO Int

ru, ri :: C.ByteString -> Int

ru = C.foldl' (\a c -> a * 10 - 48 + ord c) 0
ri s
  | C.head s == '-' = negate $ ru $ C.tail s
  | otherwise = ru s

solve :: [Int] -> Builder
solve (n:a) = intDec res <> char7 '\n'
  where
    b = take n a
    !m = maximum b
    x :: UA.UArray Int Int
    x = UA.accumArray (+) 0 (0, m) (zip b (repeat 1))
    !res = runST $ do
      c <- thaw x :: ST s (STUArray s Int Int)
      let 
        loop k s = do
          if s > m then return $! (k - 1)
          else do
            w <- readArray c s
            if w > 0 then do
              writeArray c s (pred w)
              loop (succ k) (max (succ k) s)
            else loop k (succ s)
      loop 1 1

main :: IO ()
main = hPutBuilder stdout . solve . map ri . C.words =<< C.getContents

