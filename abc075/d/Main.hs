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

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | x:ys <- tails xs, y:zs <- tails ys]

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

type Point     = (Int, Int)
type Rectangle = (Point, Point)

rectangles :: [Point] -> [Rectangle]
rectangles ps = [((p, q), (r, s)) | (p, r) <- pairs (xvalues ps)
                                  , (q, s) <- pairs (yvalues ps)]
  where
    xvalues = sort . map fst
    yvalues = sort . map snd

contains :: Int -> Rectangle -> [Point] -> Bool
contains k ((p, q), (r, s)) = (>= k) . count inside
  where
    inside (x, y) = inRange (p, r) x && inRange (q, s) y

area :: Rectangle -> Int
area ((p, q), (r, s)) = (r - p) * (s - q)

-- https://blog.hamayanhamayan.com/entry/2017/10/15/002250
solve :: Int -> [Point] -> Int
solve k ps = minimum [area r | r <- rectangles ps, contains k r ps]

main :: IO ()
main = do
  [n, k] <- readIntList
  ps     <- readTuples n
  print $ solve k ps
