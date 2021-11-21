{-
    fast read functions with ByteString
-}

{-# Options_GHC -O2 #-}
import Data.Char
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.List
import Text.Printf

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

getStrLn = BS.unpack <$> BS.getLine


putListLn = \x -> putStrLn $ tail $ x >>=  (' ':) . show

    
main = do
    a <- getInt
    b <- getIntList
    c <- getStrLn
    putListLn b
