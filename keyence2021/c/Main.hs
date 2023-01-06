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

-- rows :: CMatrix -> [[Char]]
-- rows = map (map snd) . groupBy ((==) `on` fst . fst) . assocs

-- solve :: Int -> Int -> Int -> CMatrix -> Int
-- solve h w k cs = modPow 3 (h*w-k) `mmul` head (foldr rowstep row css)
--   where
--     d                   = 2 `mmul` (fromJust $ modInv 3 modulo)
--     row                 = replicate (w-1) 0 ++ [1]
--     css                 = rows cs
--     rowstep cs row      = scanr colstep 0 (zip cs row)
--     colstep (c, dp) acc = case c of
--                             'D' -> dp
--                             'R' -> acc
--                             'X' -> dp `madd` acc
--                             ' ' -> md $ d * (dp + acc)

-- -- https://atcoder.jp/contests/keyence2021/editorial/564
-- solve :: Int -> Int -> Int -> CMatrix -> Int
-- solve h w k cs = modPow 3 (h*w-k) `mmul` dp
--   where
--     d  = 2 `mmul` (fromJust $ modInv 3 modulo)
--     dp = build 1 (replicate (w+1) 0)
--     build i xs
--       | i > h     = last xs
--       | otherwise = last ys `seq` build (i+1) ys
--       where
--         ys = scanl' step 0 $ zip (tail xs) [1..]
--         step y (x, j)
--           | i == 1 && j == 1 = 1
--           | otherwise        = y * factor (i, j-1) 'R' `madd` x * factor (i-1, j) 'D'
--     factor (i, j) c
--       | i == 0 || j == 0                   = 0
--       | cs!(i, j) == 'X' || cs!(i, j) == c = 1
--       | cs!(i, j) == ' '                   = d
--       | otherwise                          = 0

-- -- Row major index: (1, 1) <--> 1, (h, w) <--> h*w
-- rmi :: Int -> Cell -> Int
-- rmi w (i, j) = (i-1)*w + j
-- {-# INLINE rmi #-}

-- https://atcoder.jp/contests/keyence2021/editorial/564
-- https://atcoder.jp/contests/keyence2021/submissions/19515050
solve :: Int -> Int -> Int -> CMatrix -> Int
solve h w k cs = modPow 3 (h*w-k) `mmul` calc
  where
    d    = 2 `mmul` (fromJust $ modInv 3 modulo)
    cs' = listArray (1, h*w) (elems cs)
    calc = runST $ do
      dp <- UMV.replicate (h*w+1) (0 :: Int)
      UMV.write dp 1 1
      let modify i x = when (i <= h*w) $ UMV.modify dp (`madd` x) i
          dist 0 _   = return ()
          dist i x   = case cs'!i of
                         'D' -> modify x (i+w)
                         'R' -> modify x (i+1)
                         'X' -> modify x (i+w) >> modify x (i+1)
                         ' ' -> modify (d*x) (i+w) >> modify (d*x) (i+1)
      UMV.imapM_ dist dp
      UMV.read dp (h*w)
 
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
