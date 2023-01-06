{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE BangPatterns #-}
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
import Control.Monad.ST
-- import Control.Monad.State
import Data.Array
-- import Data.Array.ST
import Data.Bits
import Data.Char
-- import Data.Foldable
import Data.Function
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
import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

modulo :: Int
modulo = 998244353

md :: Int -> Int
md x = x `mod` modulo

infixl 6 `madd`
madd :: Int -> Int -> Int
madd x y = (x+y) `mod` modulo

infixl 7 `mmul`
mmul :: Int -> Int -> Int
mmul x y = (x*y) `mod` modulo

modPow :: Int -> Int -> Int
modPow x 0 = 1
modPow x n = x `mmul` modPow x (n-1)

-- https://rosettacode.org/wiki/Modular_inverse
modInv :: Int -> Int -> Maybe Int
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

type Cell     = (Int, Int)
type Matrix a = Array Cell a
type CMatrix  = Matrix Char

single :: [a] -> Bool
single [x] = True
single _   = False

-- https://atcoder.jp/contests/keyence2021/editorial/564
solve :: Int -> Int -> Int -> CMatrix -> Int
solve h w k cs = modPow 3 (h*w-k) `mmul` dp
  where
    d               = 2 `mmul` (fromJust $ modInv 3 modulo)
    (_, _, dp)      = head $ until single step (step [(1, 1, 1)])
    step            = merge . concatMap distribute
    merge []          = []
    merge [(i, j, v)] = if j <= w then [(i, j, v)] else []
    merge ((i, j, v):(k, l, w):rest)
      | i > h     = merge ((k, l, w):rest)
      | i == k    = (i, j, v `madd` w):merge rest -- (i, j) == (k, l) <=> i == k (because of invariant: i+j == k+l)
      | otherwise = (i, j, v):merge ((k, l, w):rest)
    distribute (i, j, v)
      = case cs!(i, j) of
          'D' -> [(i+1, j, v)]
          'R' -> [(i, j+1, v)]
          'X' -> [(i+1, j, v), (i, j+1, v)]
          ' ' -> [(i+1, j, d `mmul` v), (i, j+1, d `mmul` v)]

-- -- ver. 2
-- solve :: Int -> Int -> Int -> CMatrix -> Int
-- solve h w k cs = modPow 3 (h*w-k) `mmul` dp
--   where
--     d               = 2 `mmul` (fromJust $ modInv 3 modulo)
--     (_, _, dp) = head $ until single (extend 0 []) [(1, 1, 1)]
--     step downBuf x@(i, j, v) [] = if valid x then [x] else 
      
readCMatrix :: Int -> Int -> Int -> IO CMatrix
readCMatrix h w k
  = array ((1, 1), (h, w)) . (zip (range ((1, 1), (h, w))) (repeat ' ') ++)
    <$> replicateM k (do
                         [h, w, c] <- words <$> getLine
                         return ((read h, read w), head c))

main :: IO ()
main = do
  [h, w, k] <- readIntList
  cs        <- readCMatrix h w k
  print $ solve h w k cs
