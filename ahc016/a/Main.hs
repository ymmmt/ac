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

minimumOn :: Ord a => (b -> a) -> [b] -> (b, a, Int)
minimumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx <= ky then u else v

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

d2 :: [Double] -> [Double] -> Double
d2 xs ys = sum $ zipWith d2' xs ys
  where d2' x y = (x - y) ** 2

type Graph   = Array (Index, Index) Bool
type Size    = Int
type Degree  = Int
type Edge    = (Index, Index)
type Index   = Int

type Epsilon = Double

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

approximateRegularG :: Size -> Degree -> State StdGen Graph
approximateRegularG n d = do
  let p = fromIntegral d / fromIntegral (n-1)
  bs <- replicateM (n * (n-1) `div` 2) $ judgeSt p
  let g = accumArray (||) False ((0, 0), (n-1, n-1))
          $ zip (edges n) bs
  return g

lindiv :: Size -> Int -> Int
lindiv n m = n * (n-1) `div` 2 `div` m

aRGs :: Size -> Int -> State StdGen [Graph]
aRGs n m = mapM (approximateRegularG n) [0, d..d*(m-1)]
  where d = n `div` m

solveSt :: Int -> Epsilon -> State StdGen (Size, [Graph])
solveSt m e = (n,) <$> aRGs n m
  where n = 100

expectedDegree :: Epsilon -> Size -> Degree -> Double
expectedDegree e n d = (1 - 2 * e) * d' + (n' - 1) * e
  where n' = fromIntegral n
        d' = fromIntegral d

dist :: Epsilon -> Size -> Graph -> Graph -> Double
dist e n g h = d2 ds es
  where ds = map fromIntegral $ degrees n g
        es = map (expectedDegree e n) $ degrees n h

guess :: Epsilon -> Size -> Int -> Graph -> [Graph] -> Index
guess e n m g gs = i
  where (_, _, i) = minimumOn (dist e n g) gs

simulateSt :: Epsilon -> Size -> Graph -> State StdGen Graph
simulateSt e n g = do
  bs <- replicateM (n * (n-1) `div` 2) $ judgeSt e
  i  <- randomRSt (0, n-1)
  let es  = (edges n)
      es' = (permutations es)!!i
      g'  = accum xor g $ zip (edges n) bs
      g'' = accumArray (||) False ((0, 0), (n-1, n-1))
            . zip es $ map (g'!) es'
  return g''

answer :: Epsilon -> Size -> Int -> [Graph] -> IO ()
answer e n m gs = do
  g <- (parseG n) <$> getLine
  print $ guess e n m g gs
  flush

debugAnswer :: Epsilon -> Size -> Int -> [Graph] -> IO ()
debugAnswer e n m gs = do
  i      <- readInt
  (g, s) <- runState (simulateSt e n (gs!!i)) <$> newStdGen
  -- print $ "guessing: " ++ show i
  -- putStrLn $ (showG n) g
  print $ guess e n m g gs
  flush

main :: IO ()
main = do
  [sm, se] <- getWords
  let m = read sm :: Int
      e = read se :: Epsilon
  ((n, gs), _) <- runState (solveSt m e) <$> newStdGen
  putStr . unlines $ [show n] ++ map (showG n) gs
  flush
  replicateM_ 100 (answer e n m gs)
--  replicateM_ 100 (debugAnswer e n m gs)
