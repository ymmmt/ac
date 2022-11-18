{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Data.Function
import Data.Int
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Void
import Data.Word
import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
import qualified Data.Map as Map
import qualified Data.Ratio as R
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Numeric as N

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

readTuples :: Int -> IO [(Int, Int)]
readTuples n = replicateM n (t <$> readIntList)
  where t [x, y] = (x, y)

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

type MultiSet a = [(a, Int)]

multiSet :: Ord a => [a] -> MultiSet a
multiSet = map (pair (head, length)) . group . sort

toList :: MultiSet a -> [a]
toList = concatMap (\(x, n) -> replicate n x)

groupSeq :: (a -> a -> Bool) -> [a] -> [[a]]
groupSeq _ []     = []
groupSeq _ [x]    = [[x]]
groupSeq f (x:xs) = if f x y then (x:y:ys):yss else [x]:gs
  where gs@((y:ys):yss) = groupSeq f xs

maxDiscard :: Int -> Int -> MultiSet Int -> Int
maxDiscard n m xs = if null $ tail ss
                    then head ss `div` 2
                    else maximum ss
  where
    isSeq (x, _) (y, _) = (x + 1) `mod` m == y
    ss                  = map (sum . toList) $ groupSeq isSeq xs

solve :: Int -> Int -> [Int] -> Int
solve n m as = sum as - (maxDiscard n m $ xs ++ xs)
  where xs = multiSet as

main :: IO ()
main = do
  [n, m] <- readIntList
  as     <- readIntList
  print $ solve n m as
  
