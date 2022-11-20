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
import System.Random -- random-1.0.1.1
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

type Graph   = Array (Index, Index) Bool
type Size    = Int
type Degree  = Int
type Edge    = (Index, Index)
type Index   = Int

type Epsilon = Double
type Feature = Double

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

kEdgeG :: Size -> Int -> Graph
kEdgeG n k = accumArray (||) False ((0, 0), (n-1, n-1))
             $ zip (edges n) (replicate k True ++ repeat False)

lindiv :: Size -> Int -> Int
lindiv n m = n * (n-1) `div` 2 `div` m

solve :: Int -> Epsilon -> (Size, [Graph])
solve m e = (n, gs)
  where n  = 100
        d  = lindiv n m
        gs = map (kEdgeG n) [0, d..d*(m-1)]

feature :: Graph -> Feature
feature = fromIntegral . length . filter id . elems

-- サイズn、辺数kのグラフが確率eでランダム変異したときの辺数の期待値
expectedEdges :: Epsilon -> Size -> Graph -> Feature
expectedEdges e n g = k * (1 - 2 * e) + n' * (n' - 1) * e / 2
  where n' = fromIntegral n
        k  = feature g

dist :: Double -> Double -> Double
dist x y = abs (x - y)

guess :: Epsilon -> Size -> Int -> Graph -> [[Feature]] -> Index
guess e n m g fss = i
  where (_, _, i)  = minimumOn minDist fss
        fg         = feature g
        minDist fs = minimum $ map (dist fg) fs

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
  let g'  = accum xor g $ zip es bs
  return g'

simulateCount :: Int
simulateCount = 10

randomGsSt :: Epsilon -> Size -> Graph -> State StdGen [Graph]
randomGsSt e n g = replicateM simulateCount $ simulateSt' e n g

answer :: Epsilon -> Size -> Int -> [[Feature]] -> IO ()
answer e n m fss = do
  g <- (parseG n) <$> getLine
  print $ guess e n m g fss
  flush

debugAnswer :: Epsilon -> Size -> Int -> [Graph] -> [[Feature]] -> IO ()
debugAnswer e n m gs fss = do
  i      <- readInt
  (g, _) <- runState (simulateSt e n (gs!!i)) <$> newStdGen
  print $ guess e n m g fss
  flush

main :: IO ()
main = do
  [sm, se] <- getWords
  let m       = read sm :: Int
      e       = read se :: Epsilon
      (n, gs) = solve m e
  putStr . unlines $ [show n] ++ map (showG n) gs
  flush

  s <- newStdGen
  let (gss, _) = runState (mapM (randomGsSt e n) gs) s
      fss      = map (map feature) gss
--  replicateM_ 100 (answer e n m fss)
  replicateM_ 100 (debugAnswer e n m gs fss)
