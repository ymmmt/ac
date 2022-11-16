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
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
import qualified Data.Map as Map
import qualified Data.Ratio as R
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Numeric as N

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

randomUniformRSt :: (RandomGen g, UniformRange a) => (a, a) -> State g a
randomUniformRSt xy = state (uniformR xy)

new :: a -> a -> a
new o n = n

minimumOn :: Ord a => (b -> a) -> [b] -> (b, a, Int)
minimumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx <= ky then u else v

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

type Size  = Int
type Index = Int
type Graph = Array (Index, Index) Bool
type Edge  = (Index, Index)
type Epsilon = Double

edges :: Size -> [Edge]
edges n = [(i, j) | i <- [0..n-2], j <- [i+1..n-1]]

charB :: Bool -> Char
charB True  = '1'
charB False = '0'

boolC :: Char -> Bool
boolC '1' = True
boolC '0' = False

showG :: Size -> Graph -> String
showG n g = map (charB . (g!)) $ edges n

parseG :: Size -> String -> Graph
parseG n s = accumArray (||) False ((0, 0), (n-1, n-1))
             $ zip (edges n) (map boolC s)

feature :: Graph -> Double
feature = fromIntegral . length . filter id . elems

-- サイズn、辺数kのグラフが確率eでランダム変異したときの辺数の期待値
expectedEdges :: Epsilon -> Size -> Graph -> Double
expectedEdges e n g = k * (1 - 2 * e) + n' * (n' - 1) * e / 2
  where n' = fromIntegral n
        k  = feature g

diff :: Epsilon -> Size -> Graph -> Graph -> Double
diff e n g h = abs (fg - fh)
  where fg = feature g
        fh = expectedEdges e n h

kEdgeG :: Size -> Int -> State StdGen Graph
kEdgeG n k = do
  let g = accumArray (||) False ((0, 0), (n-1, n-1))
          $ zip (edges n) (replicate k True ++ repeat False)
  return g

-- randomG :: Size -> State StdGen Graph
-- randomG n = do
--   bs <- replicateM (n * (n-1) `div` 2) randomSt
--   return $ accumArray (||) False ((0, 0), (n-1, n-1)) $ zip (edges n) bs

solve :: Int -> Epsilon -> State StdGen (Size, [Graph])
solve m e = do
  let n = 100
      d = n * (n-1) `div` 2 `div` m
--  gs <- replicateM m (randomG n)
  gs <- mapM (kEdgeG n) [0, d..d*(m-1)]
  return (n, gs)

guess :: Epsilon -> Size -> Graph -> [Graph] -> Index
guess e n g gs = i
  where (_, _, i) = minimumOn (diff e n g) gs

simulate :: Epsilon -> Size -> Graph -> State StdGen Graph
simulate e n g = do
  ps <- replicateM (n * (n-1) `div` 2) $ randomUniformRSt (0, 1.0)
  i  <- randomUniformRSt (0, n-1)
  let bs  = map (< e) ps
      es  = (edges n)
      es' = (permutations es)!!i
      g'  = accum xor g $ zip (edges n) bs
      g'' = accumArray (||) False ((0, 0), (n-1, n-1))
            $ zip es $ map (g'!) es'
  return g''

answer :: Epsilon -> Size -> [Graph] -> IO ()
answer e n gs = do
  g <- (parseG n) <$> getLine
  print $ guess e n g gs
  flush

debugAnswer :: Epsilon -> Size -> [Graph] -> IO ()
debugAnswer e n gs = do
  i      <- readInt
  (g, _) <- runState (simulate e n (gs!!i)) <$> newStdGen
  -- print $ "guessing: " ++ show i
  -- putStrLn $ (showG n) g
  -- putStrLn $ "num of edges: " ++ show (feature g)
  print $ guess e n g gs
  flush

main :: IO ()
main = do
  [sm, se] <- getWords
  let m = read sm :: Int
      e = read se :: Epsilon
  ((n, gs), _) <- runState (solve m e) <$> newStdGen
  putStr . unlines $ [show n] ++ map (showG n) gs
  flush
--  replicateM_ 100 (answer e n gs)
  replicateM_ 100 (debugAnswer e n gs)
