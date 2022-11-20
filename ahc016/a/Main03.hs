{-# OPTIONS_GHC -O2 #-}

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
import Control.Monad.State
import Data.Array
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
import System.IO
import System.Random hiding (Finite) -- random-1.0.1.1
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
import qualified Data.Map as Map
import qualified Data.Ratio as R
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Numeric as N

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

getWords :: IO [String]
getWords = words <$> getLine

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

flush :: IO ()
flush = hFlush stdout

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt xy = state (randomR xy)

type Probability = Double

judgeSt :: Probability -> State StdGen Bool
judgeSt p = (< p) <$> randomRSt (0.0, 1.0)

shuffleStepSt :: Map.Map Int a -> (Int, a) -> State StdGen (Map.Map Int a)
shuffleStepSt mp (i, x) = do
  j <- randomRSt (0, i)
  return . Map.insert j x $ Map.insert i (mp Map.! j) mp

shuffleSt :: [a] -> State StdGen [a]
shuffleSt [] = return []
shuffleSt xs = do
  mp <- foldM shuffleStepSt (Map.singleton 0 (head xs)) $ zip [1..] (tail xs)
  return $ Map.elems mp

minimumOn :: Ord a => (b -> a) -> [b] -> (b, a, Int)
minimumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx <= ky then u else v

maximumOn :: Ord a => (b -> a) -> [b] -> (b, a, Int)
maximumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx >= ky then u else v

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

d2 :: [Double] -> [Double] -> Double
d2 xs ys = sum $ zipWith d2' xs ys
  where d2' x y = (x - y) ** 2

type IFactors = [Int]
type DFactors = [Double]

fact :: Int -> IFactors
fact n = [1..n]

fact' = memoize fact

choose :: Int -> Int -> (IFactors, IFactors)
choose n k = (fact n, fact k ++ fact (n - k))

choose' = memoize2 choose

power :: Double -> Int -> DFactors
power x d = replicate d x

-- compute (choose n k * x^k * (1-x)^(n-k))
term :: Int -> Int -> Double -> Double
term n k x
  | n < k || n < 0 || k < 0 || x == 0 = 0
  | otherwise = product $ zipWith3 f ps qs xs
  where
    (ps, qs) = choose n k
    xs       = power x k ++ power (1-x) (n-k)
    f p q x  = p' * x / q'
      where p' = fromIntegral p
            q' = fromIntegral q

type Graph    = Array (Index, Index) Bool
type Size     = Int
type Degree   = Int
type Edge     = (Index, Index)
type Index    = Int

type Epsilon  = Double
type NumEdges = Int

charB :: Bool -> Char
charB True  = '1'
charB False = '0'

boolC :: Char -> Bool
boolC '1' = True
boolC '0' = False
boolC _   = undefined

showG :: Size -> Graph -> String
showG n g = map (charB . (g!)) $ edges n

parseG :: Size -> String -> Graph
parseG n s = accumArray (||) False ((0, 0), (n-1, n-1))
             $ zip (edges n) (map boolC s)

edges :: Size -> [Edge]
edges n = [(i, j) | i <- [0..n-2], j <- [i+1..n-1]]

degrees :: Size -> Graph -> [Degree]
degrees n g = sort $ map deg [0..n-1]
  where deg i = count (g!) [(min i j, max i j) | j <- [0..n-1], j /= i]

kEdgeG :: Size -> NumEdges -> Graph
kEdgeG n k = accumArray (||) False ((0, 0), (n-1, n-1))
             $ zip (edges n) (replicate k True ++ repeat False)

lindiv :: Size -> Int -> Int
lindiv n m = n * (n-1) `div` 2 `div` m

edgeNums :: Size -> Int -> [NumEdges]
edgeNums n m = [0, u..u*(m-1)]
  where u = lindiv n m

solve :: Int -> Epsilon -> (Size, [Graph])
solve m e = (n, gs)
  where n  = 100
        gs = map (kEdgeG n) $ edgeNums n m

numEdges :: Graph -> NumEdges
numEdges = fromIntegral . length . filter id . elems

-- サイズn、辺数kのグラフが確率eでランダム変異したときの辺数の期待値
eNumEdges :: Epsilon -> Size -> NumEdges -> Double
eNumEdges e n k = k' * (1 - 2 * e) + n' * (n' - 1) * e / 2
  where n' = fromIntegral n
        k' = fromIntegral k

dist :: Double -> Double -> Double
dist x y = abs (x - y)

-- 辺数dのグラフが辺数d'のグラフにランダム変異する確率
prob :: Epsilon -> Size -> NumEdges -> NumEdges -> Probability
prob e n d d' = sum $ map prob' [max 0 (d - d')..min d (s - d')]
  where
    -- 元のグラフのd本の辺のうちc本が無くなる確率
    s       = n * (n - 1) `div` 2
    prob' c = term d c e * term (s - d') (d' - d + c) e

guess :: Epsilon -> Size -> Int -> Graph -> Index
guess e n m g = i
  where
    ds           = edgeNums n m
    u            = lindiv n m
    d'           = numEdges g
    (d'', _, i)  = minimumOn (\d -> dist (fromIntegral d') (eNumEdges e n d)) ds
    (d''', _, _) = maximumOn (\d -> prob e n d d')
                   . filter (inRange (0, u*(m-1)))
                   $ [d''-u, d'', d''+u]
    i'           = d''' `div` u

simulateSt :: Epsilon -> Size -> Graph -> State StdGen Graph
simulateSt e n g = do
  let es = edges n
  bs  <- replicateM (n * (n-1) `div` 2) $ judgeSt e
  es' <- shuffleSt es
  let g'  = accum xor g $ zip es bs
      g'' = accumArray (||) False ((0, 0), (n-1, n-1))
            . zip es $ map (g'!) es'
  return g''

-- simulate only edge fliping
simulateSt' :: Epsilon -> Size -> Graph -> State StdGen Graph
simulateSt' e n g = do
  let es = edges n
  bs  <- replicateM (n * (n-1) `div` 2) $ judgeSt e
  let g' = accum xor g $ zip es bs
  return g'

answer :: Epsilon -> Size -> Int -> IO ()
answer e n m = do
  g <- (parseG n) <$> getLine
  print $ guess e n m g
  flush

debugAnswer :: Epsilon -> Size -> Int -> [Graph] -> IO ()
debugAnswer e n m gs = do
  i      <- readInt
  (g, _) <- runState (simulateSt e n (gs!!i)) <$> newStdGen
  print $ guess e n m g
  flush

main :: IO ()
main = do
  [sm, se] <- getWords
  let m       = read sm :: Int
      e       = read se :: Epsilon
      (n, gs) = solve m e
  putStr . unlines $ [show n] ++ map (showG n) gs
  flush

  replicateM_ 100 (answer e n m)
--  replicateM_ 100 (debugAnswer e n m gs)
