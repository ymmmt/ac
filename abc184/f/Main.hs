{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeFamilies #-}

-- import Control.Applicative ((<*>), Alternative, empty, (<|>))
import Control.Monad
-- import Control.Monad.ST
import Data.Array
-- import Data.Array.ST
-- import Data.Bits
import Data.Char
-- import Data.Function
-- import Data.Int
-- import Data.Ix
import Data.List
import Data.Maybe
-- import Data.Tuple
-- import Data.Void
-- import Data.Word
-- import Debug.Trace
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

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

psSums :: (Ord a, Num a) => [a] -> [a]
psSums []     = [0]
psSums (x:xs) = merge ys $ map (+x) ys
  where ys = psSums xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> x:merge xs' ys
    EQ -> x:y:merge xs' ys'
    GT -> y:merge xs ys'

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f l r = if l < r && f l then go l r
                else error "bsearch: invalid arguments"
  where go l r
          | l+1 == r  = l
          | f m       = go m r
          | otherwise = go l m
          where m = (l+r) `div` 2

solve :: Int -> Int -> [Int] -> Int
solve n t as = maximum $ map find vs
  where
    m        = n `div` 2
    (xs, ys) = splitAt m as
    us       = UA.listArray (1, 2^m) $ psSums xs :: UA.UArray Int Int
    vs       = filter (<=t) $ psSums ys
    find v   = v + us UA.! i
      where i = bsearch ((<=t-v) . (us UA.!)) 1 (2^m+1)

main :: IO ()
main = do
  [n, t] <- readIntList
  as     <- readIntList
  print $ solve n t as
