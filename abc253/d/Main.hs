{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Vector.Unboxed (Vector, (!), (//), fromList)
import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

arithSeqSum :: Integral a => a -> a -> a -> a
arithSeqSum n d a0 = n * a0 + d * n * (n-1) `div` 2

multiplesSum :: Int -> Int -> Int
multiplesSum n x = arithSeqSum (n `div` x) x x        

solve :: Int -> Int -> Int -> Int
solve n a b = multiplesSum n 1 - multiplesSum n a - multiplesSum n b + multiplesSum n (lcm a b)

main :: IO ()
main = do
  [n, a, b] <- readIntList
  print $ solve n a b
