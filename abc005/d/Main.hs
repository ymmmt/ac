{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeFamilies #-}

-- import Control.Applicative ((<*>), Alternative, empty, (<|>))
import Control.Monad
-- import Control.Monad.ST
import Data.Array
-- import Data.Array.ST
-- import Data.Bits
import Data.Char
-- import Data.Foldable
-- import Data.Function
-- import Data.Int
-- import Data.Ix
import Data.List
import Data.Maybe
-- import Data.Tuple
-- import Data.Void
-- import Data.Word
import Debug.Trace
-- import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Graph as G
-- import qualified Data.Map as Map
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

type Cell     = (Int, Int)
type Matrix a = Array Cell a

readMatrix :: Read a => Int -> Int -> IO (Matrix a)
readMatrix h w = listArray ((1, 1), (h, w)) . concatMap (map read . words) <$> replicateM h getLine

-- rs: rect sum
solve :: Int -> Matrix Int -> [Int] -> [Int]
solve n d ps = map ans ps
  where
    ans        = maximum . rectSums
    rectSums p = [rs!(r+h-1, c+w-1) - rs!(r+h-1, c-1) - rs!(r-1, c+w-1) + rs!(r-1, c-1)
                 | h <- [1..min n p], let w = min n (p `div` h), r <- [1..n-h+1], c <- [1..n-w+1]]
    rs         = listArray ((0, 0), (n, n)) $ build 1 [replicate (n+1) 0]
    build r xss@(xs:_)
      | r > n     = concat (reverse xss)
      | otherwise = build (r+1) (ys:xss)
      where ys = zipWith (+) xs $ scanl (+) 0 [d!(r, c) | c <- [1..n]]

main :: IO ()
main = do
  n  <- readInt
  d  <- readMatrix n n
  q  <- readInt
  ps <- replicateM q readInt
  putStrLns $ solve n d ps
