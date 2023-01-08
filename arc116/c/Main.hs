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

tup :: [a] -> (a, a)
tup [x, y] = (x, y)

list :: (a, a) -> [a]
list (x, y) = [x, y]

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

putStrLns :: Show a => [a] -> IO ()
putStrLns = putStr . unlines . map show

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
modPow x n = mmul x $ modPow x (n-1)

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

-- make smallest prime factors vector
mkspfv :: Int -> Array Int Int
mkspfv n
  | n >= 1 = array (1, n) $ concatMap mulAssocs [n, n-1..2] ++ [(1, 1)]
  where mulAssocs k = [(d, k) | d <- [k, 2*k..n]]

pfactors :: Array Int Int -> Int -> [Int]
pfactors _    1 = []
pfactors spfv n = p:pfactors spfv (n `div` p)
  where p = spfv!n

tabulate :: Ix i => (i -> e) -> (i, i) -> Array i e
tabulate f bounds = array bounds [(x, f x) | x <- range bounds]

modFacts :: Int -> Array Int Int
modFacts n = fs
  where
    fs  = tabulate f (0, n)
    f n = if n <= 1 then 1 else n `mmul` fs!(n-1)

solve :: Int -> Int -> Int
solve n m = foldl1' madd $ map patterns [1..m]
  where
    spfv        = mkspfv m
    patterns    = foldl' mmul 1 . map (h . length) . group . pfactors spfv
    -- h: multichoose
    h d         = facts!(n+d-1) `mmul` ifacts!d `mmul` ifacts!(n-1)
    facts       = modFacts (n+m)
    ifacts      = tabulate g (0, max n m)
      where g n = fromJust $ modInv (facts!n) modulo

main :: IO ()
main = do
  [n, m] <- readIntList
  print $ solve n m
