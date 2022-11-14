{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Binary
import GHC.Generics (Generic)
-- import Control.Applicative
import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Data.Function
import Data.Int
import Data.Ix
import Data.List hiding (insert)
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

-- Data.Trie

-- |
-- Module      :  Data.Trie
-- License     :  GPL-2
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of a trie over a words. Properties:
--
-- @
-- 'fromList' . 'toList' ≡ 'id'
-- 'toList' . 'fromString' ≡ (:[])
-- 'sort' . 'nub' . 'toList' . 'fromList' ≡ 'sort' . 'nub'
-- @

-- module Data.Trie ( empty, insert, fromString, fromList
--                  , toList, lookupPrefix, forcedNext, Trie
--                  , possibleSuffixes, certainSuffix
--                  ) where

data Trie = Trie Bool (Map.Map Char Trie) deriving (Show, Eq, Generic)

-- | A blank Trie
empty :: Trie
empty = Trie False Map.empty

-- | Insert a new string into the trie.
insert :: String -> Trie -> Trie
insert []     (Trie _ m) = Trie True m
insert (x:xs) (Trie b m) =
  Trie b $ Map.alter (Just . maybe (fromString xs) (insert xs)) x m

fromString :: String -> Trie
fromString =
  foldr (\x xs -> Trie False (Map.singleton x xs)) (Trie True Map.empty)

-- | Take a list of String and compress it into a Trie
fromList :: [String] -> Trie
fromList = foldr insert empty

-- | Take a trie and expand it into the strings that it represents
toList :: Trie -> [String]
toList (Trie b m) =
    if b then "":expand
    else expand
    where expand = [ char:word | (char, trie) <- Map.toList m,
                                 word <- toList trie ]

-- | Takes a trie and a prefix and returns the sub-trie that
-- of words with that prefix
lookupPrefix :: (MonadPlus m) => String -> Trie -> m Trie
lookupPrefix [] trie = return trie
lookupPrefix (x:xs) (Trie _ m) = liftMaybe (Map.lookup x m) >>= lookupPrefix xs

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe Nothing = mzero
liftMaybe (Just x) = return x

-- | Finds the longest certain path down the trie starting at a the root
-- Invariant Assumption: All paths have at least one 'true' node below them
forcedNext :: Trie -> String
forcedNext (Trie _ m) =
    if length ls == 1 then
        let (char, trie) = head ls in
        char:forcedNext trie
    else []
    where ls = Map.toList m

-- | Helper function, finds all the suffixes of a given prefix
possibleSuffixes :: String -> Trie -> [String]
possibleSuffixes prefix fulltrie =
    lookupPrefix prefix fulltrie >>= toList

-- | Helper function, finds the longest certain path down the trie
-- starting at a given word
certainSuffix :: String -> Trie -> String
certainSuffix prefix fulltrie =
    lookupPrefix prefix fulltrie >>= forcedNext

instance Binary Trie

--

qt :: Bool -> Int
qt True  = 1
qt False = 0

-- sと等しいか辞書順でsより必ず小さいStringの数
less :: String -> Trie -> Int
less [] _ = 1
less (c:cs) (Trie b mp) = qt b + (less cs $ fromJust $ Map.lookup c mp)

-- 辞書順でsより必ず大きいStringの数
greater :: String -> Trie -> Int
greater s t = fromJust $ childNodes <$> lookupPrefix s t
  where childNodes t@(Trie b _) = nodes t - qt b
        nodes (Trie b mp)       = qt b + (sum . map nodes $ Map.elems mp)

modulo :: Int
modulo = 998244353

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

-- https://atcoder.jp/contests/abc268/editorial/4782
solve :: Int -> [String] -> [Int]
solve n ss = map ex ss
  where t    = fromList ss
        two  = fromJust $ modInv 2 modulo
        ex s = let a = less s t
                   b = greater s t
               in (a - b + n) * two `mod` modulo

main :: IO ()
main = do
  n  <- readInt
  ss <- replicateM n getLine
  putStr . unlines . map show $ solve n ss
