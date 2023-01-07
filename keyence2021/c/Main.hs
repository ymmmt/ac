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
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Graph as G
-- import qualified Data.Map as M
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
-- import qualified Data.Tuple.Strict as ST
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
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

-- single :: [a] -> Bool
-- single [x] = True
-- single _   = False

-- https://atcoder.jp/contests/keyence2021/editorial/564

-- -- ver.1
-- solve :: Int -> Int -> Int -> CMatrix -> Int
-- solve h w k cs = modPow 3 (h*w-k) `mmul` dp
--   where
--     d                 = 2 `mmul` (fromJust $ modInv 3 modulo)
--     (_, _, dp)        = head $ until single step (step [(1, 1, 1)])
--     step              = merge . concatMap distribute
--     merge []          = []
--     merge [(i, j, v)] = if j <= w then [(i, j, v)] else []
--     merge ((i, j, v):(k, l, w):rest)
--       | i > h            = merge ((k, l, w):rest)
--       | (i, j) == (k, l) = (i, j, v `madd` w):merge rest -- (i, j) == (k, l) <=> i == k (because of invariant: i+j == k+l)
--       | otherwise        = (i, j, v):merge ((k, l, w):rest)
--     distribute (i, j, v)
--       = case cs!(i, j) of
--           'D' -> [(i+1, j, v)]
--           'R' -> [(i, j+1, v)]
--           'X' -> [(i+1, j, v), (i, j+1, v)]
--           ' ' -> [(i+1, j, dv), (i, j+1, dv)]
--             where dv = d `mmul` v

-- -- ver.2: encode cells
-- solve :: Int -> Int -> Int -> CMatrix -> Int
-- solve h w k cs = modPow 3 (h*w-k) `mmul` dp
--   where
--     d          = 2 `mmul` (fromJust $ modInv 3 modulo)
--     cs'        = listArray (1, h*w) $ elems cs
--     (_, dp)    = head $ until done step (step [(1, 1)])
--     done       = (== h*w) . fst . head
--     step       = merge . concatMap distribute
--     merge []   = []
--     merge [ix] = [ix]
--     merge ((i, x):(j, y):rest)
--       | i == j    = (i, x `madd` y):merge rest
--       | otherwise = (i, x):merge ((j, y):rest)
--     distribute (i, x)
--       = let down  = i+w <= h*w
--             right = i `mod` w > 0
--             dx    = d `mmul` x
--         in case cs'!i of
--              'D' | down                  -> [(i+w, x)]
--                  | otherwise             -> []
--              'R' | right                 -> [(i+1, x)]
--                  | otherwise             -> []
--              'X' | down && right         -> [(i+w, x), (i+1, x)]
--                  | down && not right     -> [(i+w, x)]
--                  | not down && right     -> [(i+1, x)]
--                  | not down && not right -> []
--              ' ' | down && right         -> [(i+w, dx), (i+1, dx)]
--                  | down && not right     -> [(i+w, dx)]
--                  | not down && right     -> [(i+1, dx)]
--                  | not down && not right -> []

-- type RMI   = Int -- Row major index
-- type Value = Int
-- type Tuple = (RMI, Value)

-- -- ver.3: fusion merge and distribute
-- solve :: Int -> Int -> Int -> CMatrix -> Int
-- solve h w k cs = modPow 3 (h*w-k) `mmul` if null dp then 0 else snd (head dp)
--   where
--     d   = 2 `mmul` (fromJust $ modInv 3 modulo)
--     cs' = listArray (1, h*w) $ elems cs
--     dp  = until done generate [(1, 1)]

--     done :: [Tuple] -> Bool
--     done ixs = null ixs || fst (head ixs) == h*w

--     generate :: [Tuple] -> [Tuple]
--     generate = go Nothing

--     go :: Maybe Tuple -> [Tuple] -> [Tuple]
--     go Nothing         []       = []
--     go (Just ixr)      []       = [ixr]
--     go Nothing         (jy:jys) = case down jy of
--                                     Nothing  -> go (right jy) jys
--                                     Just jyd -> jyd:go (right jy) jys
--     go (Just (ir, xr)) (jy:jys) = case down jy of
--                                     Nothing       -> (ir, xr):go (right jy) jys
--                                     Just (jd, yd)
--                                       | ir == jd  -> let v = xr `madd` yd
--                                                      in v `seq` (ir, v):go (right jy) jys
--                                       | otherwise -> (ir, xr):(jd, yd):go (right jy) jys

--     down :: Tuple -> Maybe Tuple
--     down (i, x) = if i+w <= h*w
--                   then case cs'!i of
--                          'D' -> Just (i+w, x)
--                          'R' -> Nothing
--                          'X' -> Just (i+w, x)
--                          ' ' -> let v = d `mmul` x
--                                 in v `seq` Just (i+w, v)
--                   else Nothing

--     right :: Tuple -> Maybe Tuple
--     right (i, x) = if i `mod` w > 0
--                    then case cs'!i of
--                           'D' -> Nothing
--                           'R' -> Just (i+1, x)
--                           'X' -> Just (i+1, x)
--                           ' ' -> let v = d `mmul` x
--                                  in v `seq` Just (i+1, v)
--                    else Nothing

type RMI   = Int -- Row major index
type Value = Int
data Pair a b = P !a !b
type Tuple = Pair RMI Value

pfst :: Pair a b -> a
pfst (P x _) = x

psnd :: Pair a b -> b
psnd (P _ y) = y

-- ver.4: use Strict Pair
solve :: Int -> Int -> Int -> CMatrix -> Int
solve h w k cs = modPow 3 (h*w-k) `mmul` if null dp then 0 else psnd (head dp)
  where
    d   = 2 `mmul` (fromJust $ modInv 3 modulo)
    cs' = listArray (1, h*w) $ elems cs
    dp  = until done generate [P 1 1]

    done :: [Tuple] -> Bool
    done ixs = null ixs || pfst (head ixs) == h*w

    generate :: [Tuple] -> [Tuple]
    generate = go Nothing

    go :: Maybe Tuple -> [Tuple] -> [Tuple]
    go Nothing    []       = []
    go (Just ixr) []       = [ixr]
    go Nothing    (jy:jys) = case down jy of
                               Nothing  -> go (right jy) jys
                               Just jyd -> jyd:go (right jy) jys
    go (Just (P ir xr)) (jy:jys)
      = case down jy of
          Nothing       -> P ir xr:go (right jy) jys
          Just (P jd yd)
            | ir == jd  -> P ir (xr `madd` yd):go (right jy) jys
            | otherwise -> P ir xr:P jd yd:go (right jy) jys

    down :: Tuple -> Maybe Tuple
    down (P i x) = if i+w <= h*w
                   then case cs'!i of
                          'D' -> Just (P (i+w) x)
                          'R' -> Nothing
                          'X' -> Just (P (i+w) x)
                          ' ' -> Just (P (i+w) (d `mmul` x))
                   else Nothing

    right :: Tuple -> Maybe Tuple
    right (P i x) = if i `mod` w > 0
                    then case cs'!i of
                           'D' -> Nothing
                           'R' -> Just (P (i+1) x)
                           'X' -> Just (P (i+1) x)
                           ' ' -> Just (P (i+1) (d `mmul` x))
                    else Nothing

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
