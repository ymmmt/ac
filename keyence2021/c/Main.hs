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
import qualified Data.Vector.Unboxed as UV
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
modPow x 1 = md x
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

-- Row major index: (1, 1) <--> 1, (h, w) <--> h*w
rmi :: Int -> Cell -> Int
rmi w (i, j) = (i-1)*w + j
{-# INLINE rmi #-}

-- https://atcoder.jp/contests/keyence2021/editorial/564
-- https://atcoder.jp/contests/keyence2021/submissions/19515050
solve :: Int -> Int -> Int -> UV.Vector Char -> Int
solve h w k cs = modPow 3 (h*w-k) `mmul` calc
  where
    d    = 2 `mmul` (fromJust $ modInv 3 modulo)
    calc = runST $ do
      dp <- UMV.replicate (h*w+1) 0
      UMV.unsafeWrite dp 1 1
      let distD i x = when (i+w <= h*w) $ UMV.unsafeModify dp (`madd` x) (i+w)
          distR i x = when (i `mod` w > 0) $ UMV.unsafeModify dp (`madd` x) (i+1)
      forM_ [1..h*w] $ \i -> do
        x <- UMV.unsafeRead dp i
        case cs UV.! i of
          'D' -> distD i x
          'R' -> distR i x
          'X' -> distD i x >> distR i x
          ' ' -> distD i (d*x) >> distR i (d*x)
      UMV.unsafeRead dp (h*w)

readCVector :: Int -> Int -> Int -> IO (UV.Vector Char)
readCVector h w k
  = UV.unsafeAccum (flip const) (UV.replicate (h*w+1) ' ')
    <$> replicateM k (do
                         [sh, sw, sc] <- words <$> getLine
                         return (rmi w (read sh, read sw), head sc))

main :: IO ()
main = do
  [h, w, k] <- readIntList
  cs        <- readCVector h w k
  print $ solve h w k cs
