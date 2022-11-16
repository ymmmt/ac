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

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

chunks :: Int -> [a] -> [[a]]
chunks _ []   = []
chunks n xs
  | n <= 0    = []
  | otherwise = ys:chunks n zs
  where (ys, zs) = splitAt n xs

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
boolC _   = undefined

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

simulateCount :: Int
simulateCount = 10

diffSt :: Epsilon -> Size -> Graph -> Graph -> State StdGen Double
diffSt e n g h = do
  hs <- replicateM simulateCount (simulateSt e n h)
  let fg = feature g
      fh = average $ map feature hs
  return $ abs (fg - fh)

kEdgeGSt :: Size -> Int -> State StdGen Graph
kEdgeGSt n k = do
  let g = accumArray (||) False ((0, 0), (n-1, n-1))
          $ zip (edges n) (replicate k True ++ repeat False)
  return g

kEdgeG :: Size -> Int -> Int -> Graph
kEdgeG n offset k =
  accumArray (||) False ((0, 0), (n-1, n-1))
  . zip (edges n)
  $ (replicate offset False) ++ (replicate k True) ++ (repeat False)

-- randomG :: Size -> State StdGen Graph
-- randomG n = do
--   bs <- replicateM (n * (n-1) `div` 2) randomSt
--   return $ accumArray (||) False ((0, 0), (n-1, n-1)) $ zip (edges n) bs

chunkSize :: Size -> Int -> Int
chunkSize n m = n * (n-1) `div` 2 `div` m

solve :: Int -> Epsilon -> (Size, [Graph])
solve m e = (n, gs)
  where gs = map (\i -> kEdgeG n (d*i) d) [0..m-1]
        n  = 100
        d  = chunkSize n m

solveSt :: Int -> Epsilon -> State StdGen (Size, [Graph])
solveSt m e = do
  let n = 100
      d = chunkSize n m
--  gs <- replicateM m (randomG n)
  gs <- mapM (kEdgeGSt n) [0, d..d*(m-1)]
  return (n, gs)

guess :: Epsilon -> Size -> Graph -> [Graph] -> Index
guess e n g gs = i
  where (_, _, i) = minimumOn (diff e n g) gs

guess2 :: Epsilon -> Size -> Int -> Graph -> Index
guess2 e n m g = i
  where (_, _, i) = maximumOn (count id) . take m . chunks d $ map (g!) (edges n)
        d         = chunkSize n m

guessSt :: Epsilon -> Size -> Graph -> [Graph] -> State StdGen Index
guessSt e n g gs = do
  ds <- mapM (diffSt e n g) gs
  let (_, _, i) = minimumOn id ds
  return i

simulateSt :: Epsilon -> Size -> Graph -> State StdGen Graph
simulateSt e n g = do
  ps <- replicateM (n * (n-1) `div` 2) $ randomRSt (0, 1.0)
  i  <- randomRSt (0, n-1)
  let bs  = map (< e) ps
      es  = (edges n)
      es' = (permutations es)!!i
      g'  = accum xor g $ zip (edges n) bs
      g'' = accumArray (||) False ((0, 0), (n-1, n-1))
            . zip es $ map (g'!) es'
  return g''

answer :: Epsilon -> Size -> Int -> [Graph] -> IO ()
answer e n m gs = do
  g <- (parseG n) <$> getLine
  print $ guess2 e n m g
--  print . fst . runState (guessSt e n g gs) <$> newStdGen
  flush

debugAnswer :: Epsilon -> Size -> Int -> [Graph] -> IO ()
debugAnswer e n m gs = do
  i      <- readInt
  (g, s) <- runState (simulateSt e n (gs!!i)) <$> newStdGen
  print $ "guessing: " ++ show i
  putStrLn $ (showG n) g
  putStrLn $ "num of edges: " ++ show (feature g)
  print $ guess2 e n m g
--  print . fst $ runState (guessSt e n g gs) s
  flush

main :: IO ()
main = do
  [sm, se] <- getWords
  let m = read sm :: Int
      e = read se :: Epsilon
      (n, gs) = solve m e
--  ((n, gs), _) <- runState (solveSt m e) <$> newStdGen
  putStr . unlines $ [show n] ++ map (showG n) gs
  flush
--  replicateM_ 100 (answer e n m gs)
  replicateM_ 100 (debugAnswer e n m gs)
