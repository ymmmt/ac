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
import Data.Foldable
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

-- Data.Function.Memoize

class Memoizable a where
  memoize :: (a -> v) -> a -> v

memoize2 :: (Memoizable a, Memoizable b) =>
            (a -> b -> v) -> a -> b -> v
memoize2 v = memoize (memoize . v)

memoize3 :: (Memoizable a, Memoizable b, Memoizable c) =>
            (a -> b -> c -> v) -> a -> b -> c -> v
memoize3 v = memoize (memoize2 . v)

memoize4 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d) =>
            (a -> b -> c -> d -> v) -> a -> b -> c -> d -> v
memoize4 v = memoize (memoize3 . v)

memoize5 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e) =>
            (a -> b -> c -> d -> e -> v) -> a -> b -> c -> d -> e -> v
memoize5 v = memoize (memoize4 . v)

memoize6 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e, Memoizable f) =>
            (a -> b -> c -> d -> e -> f -> v) ->
            a -> b -> c -> d -> e -> f -> v
memoize6 v = memoize (memoize5 . v)

memoize7 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e, Memoizable f, Memoizable g) =>
            (a -> b -> c -> d -> e -> f -> g -> v) ->
            a -> b -> c -> d -> e -> f -> g -> v
memoize7 v = memoize (memoize6 . v)

memoFix :: Memoizable a => ((a -> v) -> a -> v) -> a -> v
memoFix ff = f where f = memoize (ff f)

memoFix2 :: (Memoizable a, Memoizable b) =>
            ((a -> b -> v) -> a -> b -> v) -> a -> b -> v
memoFix2 ff = f where f = memoize2 (ff f)

memoFix3 :: (Memoizable a, Memoizable b, Memoizable c) =>
            ((a -> b -> c -> v) -> a -> b -> c -> v) -> a -> b -> c -> v
memoFix3 ff = f where f = memoize3 (ff f)

memoFix4 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d) =>
            ((a -> b -> c -> d -> v) -> (a -> b -> c -> d -> v)) ->
            a -> b -> c -> d -> v
memoFix4 ff = f where f = memoize4 (ff f)

memoFix5 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e) =>
            ((a -> b -> c -> d -> e -> v) -> (a -> b -> c -> d -> e -> v)) ->
            a -> b -> c -> d -> e -> v
memoFix5 ff = f where f = memoize5 (ff f)

memoFix6 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e, Memoizable f) =>
            ((a -> b -> c -> d -> e -> f -> v) -> (a -> b -> c -> d -> e -> f -> v)) ->
            a -> b -> c -> d -> e -> f -> v
memoFix6 ff = f where f = memoize6 (ff f)

memoFix7 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e, Memoizable f, Memoizable g) =>
            ((a -> b -> c -> d -> e -> f -> g -> v) -> (a -> b -> c -> d -> e -> f -> g -> v)) ->
            a -> b -> c -> d -> e -> f -> g -> v
memoFix7 ff = f where f = memoize7 (ff f)

traceMemoize :: (Memoizable a, Show a) => (a -> b) -> a -> b
traceMemoize f = memoize (\a -> traceShow a (f a))

data BinaryTreeCache v = BinaryTreeCache {
  btValue         :: v,
  btLeft, btRight :: BinaryTreeCache v
  }
  deriving Functor

newtype Finite a = ToFinite { fromFinite :: a }
  deriving (Eq, Bounded, Enum)

instance (Bounded a, Enum a) => Memoizable (Finite a) where
  memoize f = finiteLookup (f <$> theFinites)

theFinites :: (Bounded a, Enum a) => BinaryTreeCache a
theFinites = loop minBound maxBound
  where loop start stop = BinaryTreeCache {
          btValue = mean,
          btLeft  = loop start (pred mean),
          btRight = loop (succ mean) stop
          }
          where mean = meanFinite start stop

finiteLookup :: (Bounded a, Enum a) => BinaryTreeCache v -> a -> v
finiteLookup cache0 a0 = loop start0 stop0 cache0
  where start0 = fromEnum (minBound `asTypeOf` a0)
        stop0  = fromEnum (maxBound `asTypeOf` a0)
        a      = fromEnum a0
        loop start stop cache =
          let mean = meanFinite start stop in
            case a `compare` mean of
              EQ -> btValue cache
              LT -> loop start (pred mean) (btLeft cache)
              GT -> loop (succ mean) stop (btRight cache)

meanFinite :: (Bounded a, Enum a) => a -> a -> a
meanFinite a b = toEnum (ia `div` 2 + ib `div` 2 +
                          if odd ia && odd ib then 1 else 0)
  where ia = fromEnum a
        ib = fromEnum b

memoizeFinite :: (Enum a, Bounded a) => (a -> v) -> a -> v
memoizeFinite f = memoize (f . fromFinite) . ToFinite

instance Memoizable Int    where memoize = memoizeFinite
instance Memoizable Char   where memoize = memoizeFinite
instance Memoizable Word   where memoize = memoizeFinite
instance Memoizable Word8  where memoize = memoizeFinite
instance Memoizable Word16 where memoize = memoizeFinite
instance Memoizable Word32 where memoize = memoizeFinite
instance Memoizable Word64 where memoize = memoizeFinite

-- deriveMemoizable ''()
-- deriveMemoizable ''Bool
-- deriveMemoizable ''Ordering
-- deriveMemoizable ''Maybe
-- deriveMemoizable ''Either
-- deriveMemoizable ''[]
-- deriveMemoizable ''Complex.Complex
-- deriveMemoizable ''Version.Version

-- deriveMemoizable ''Tuple.Solo

-- deriveMemoizable ''(,)
-- deriveMemoizable ''(,,)
-- deriveMemoizable ''(,,,)
-- deriveMemoizable ''(,,,,)
-- deriveMemoizable ''(,,,,,)
-- deriveMemoizable ''(,,,,,,)
-- deriveMemoizable ''(,,,,,,,)
-- deriveMemoizable ''(,,,,,,,,)
-- deriveMemoizable ''(,,,,,,,,,)
-- deriveMemoizable ''(,,,,,,,,,,)
-- deriveMemoizable ''(,,,,,,,,,,,)

encodeInteger :: Integer -> [Int]
encodeInteger 0 = []
encodeInteger i | minInt <= i && i <= maxInt
                = [fromInteger i]
encodeInteger i = fromInteger (i .&. maxInt) : encodeInteger (i `shiftR` intBits)

decodeInteger :: [Int] -> Integer
decodeInteger = foldr op 0
  where op i i' = fromIntegral i .|. i' `shiftL` intBits

intBits :: Int
intBits = finiteBitSize (0 :: Int) - 1

minInt, maxInt :: Integer
minInt = fromIntegral (minBound :: Int)
maxInt = fromIntegral (maxBound :: Int)

instance (Eq a, Bounded a, Enum a, Memoizable b) => Memoizable (a -> b) where
  memoize = functionLookup . theFunctions

functionLookup :: (Eq a, Bounded a, Enum a, Memoizable b) =>
                  FunctionCache b v -> (a -> b) -> v
functionLookup cache f =
  fcNil (foldl fcCons cache (f <$> [minBound .. maxBound]))

theFunctions :: (Eq a, Bounded a, Enum a, Memoizable b) =>
                ((a -> b) -> v) -> FunctionCache b v
theFunctions f = FunctionCache {
  fcNil  = f undefined,
  fcCons = memoize (\b -> theFunctions (f . extend b))
  }
  where extend b g a | a == minBound = b
                     | otherwise     = g (pred a)

data FunctionCache b v
  = FunctionCache {
      fcNil  :: v,
      fcCons :: b -> FunctionCache b v
    }

--

class Group a where
  gzero    :: a
  gplus    :: a -> a -> a
  ginverse :: a -> a

instance Num a => Group (a, a) where
  gzero                 = (0, 0)
  (a, b) `gplus` (c, d) = (a+c, b+d)
  ginverse (a, b)       = (-a, -b)

-- upper exclusive
-- each RangeAdd represents a half open interval [l, r)
imos :: Group a => Int -> Int -> [(Int, Int, a)] -> [a]
imos lower upper = scanl1 gplus . Data.Foldable.toList . accumArray gplus gzero (lower, upper) . concatMap adds
  where adds (l, r, d) = [(l, d), (r, ginverse d)]

type RangeLinearFunc = (Int, Int, (Int, Int))
-- (l, r, (c1, c0)) represents y = c1 * x + c0 in [l, r)

slide :: Int -> RangeLinearFunc -> RangeLinearFunc
slide offset (l, r, (c1, c0)) = (l + offset, r + offset, (c1, c0 - c1 * offset))

-- [0,n]がちょうど一周期となる線形関数を周期内に折り返す
foldIn :: Int -> RangeLinearFunc -> [RangeLinearFunc]
foldIn n lf@(l, r, (c1, c0))
  | l > r      = undefined
  | l >= n     = [slide (-n) lf]
  | r <= 0     = [slide n lf]
  -- l < n && r > 0
  | l < 0      = [(0, r, (c1, c0)), slide n (l, 0, (c1, c0))]
  | r > n      = [(l, n, (c1, c0)), slide (-n) (n, r, (c1, c0))]
  -- 0 <= l <= r <= n
  | otherwise  = [lf]

baseRLFs :: Int -> [RangeLinearFunc]
baseRLFs n = [(0, k1, (1, 0)), (k1, k2, (0, k1)), (k2, n, (-1, n))]
  where k1 = floor (fromIntegral n/2)
        k2 = ceiling (fromIntegral n/2)

rlfs :: Int -> Array Int Int -> Int -> [RangeLinearFunc]
rlfs n poss i = concatMap (foldIn n . slide (i - poss!i)) $ baseRLFs n

solve :: Int -> [Int] -> Int
solve n ps = minimum $ zipWith (\(c1, c0) x -> c1 * x + c0) linearF [0..n-1]
  where poss    = array (0, n-1) $ zip ps [0..n-1]
        linearF = imos 0 n $ concatMap (rlfs n poss) [0..n-1]

main :: IO ()
main = do
  n  <- readInt
  ps <- readIntList
  print $ solve n ps
