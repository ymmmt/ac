{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative ((<*>), Alternative, empty, (<|>))
import Control.Monad
-- import Control.Monad.ST
import Data.Array
-- import Data.Array.ST
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
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
import qualified Data.Map as Map
import qualified Data.Ratio as R
import qualified Data.Tree as T
import qualified Data.Set as S
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Numeric as N

tup :: [Int] -> (Int, Int)
tup [x, y] = (x, y)

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

readTuples :: Int -> IO [(Int, Int)]
readTuples n = replicateM n (tup <$> readIntList)

readIntListsAll :: IO [[Int]]
readIntListsAll = map toIntList . BS.lines <$> BS.getContents

readTuplesAll :: IO [(Int, Int)]
readTuplesAll = map tup <$> readIntListsAll

joinStr :: Show a => String -> [a] -> String
joinStr sep xs = intercalate sep $ map show xs

zapp :: [(a -> b)] -> [a] -> [b]
zapp = zipWith ($)

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f l r = if l < r && f l then go l r
                else error "bsearch: invalid arguments"
  where go l r 
          | l+1 == r  = l
          | f m       = go m r
          | otherwise = go l m
          where m = (l+r) `div` 2

-- https://img.atcoder.jp/agc020/editorial.pdf
f :: Int -> Int -> Int
f p x = x - (x `mod` p)

solve :: Int -> [Int] -> [Int]
solve k as = if l <= r then [l, r] else [-1]
  where g n = foldl' (flip ($)) n fs
        fs = zapp (repeat f) as
        u = 2 + k * (maximum as)
        l = negate $ bsearch ((>= 2) . g . negate) (-u) 0
        r = bsearch ((<= 2) . g) 0 u
        
main :: IO ()
main = do
  k  <- readInt
  as <- readIntList
  putStrLn . joinStr " " $ solve k as
