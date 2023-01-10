{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeFamilies #-}

-- import Control.Applicative ((<*>), Alternative, empty, (<|>))
import Control.Monad
-- import Control.Monad.ST
import Control.Monad.State
import Data.Array
-- import Data.Array.ST
import Data.Bits
import Data.Char
-- import Data.Foldable
-- import Data.Function
-- import Data.Int
-- import Data.Ix
import Data.List
import Data.Maybe
-- import Data.Tuple
-- import Data.Tuple.Extra
-- import Data.Void
import Data.Word
import Debug.Trace
-- import System.Random -- random-1.0.1.1
-- import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Graph as G
-- import qualified Data.Map as M
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

tabulate :: Ix i => (i -> e) -> (i, i) -> Array i e
tabulate f bounds = array bounds [(x, f x) | x <- range bounds]

-- https://atcoder.jp/contests/abc185/editorial/356
solve :: Int -> Int -> [Int] -> [Int] -> Int
solve n m as bs = dp!(n, m)
  where
    a  = listArray (1, n) as
    b  = listArray (1, m) bs
    dp = tabulate count ((0, 0), (n, m))
    count (i, j)
      | i == 0    = j
      | j == 0    = i
      | otherwise = minimum [ dp!(i-1, j) + 1
                            , dp!(i, j-1) + 1
                            , dp!(i-1, j-1) + fromEnum (a!i /= b!j)]

main :: IO ()
main = do
  [n, m]   <- readIntList
  [as, bs] <- readIntLists 2
  print $ solve n m as bs
