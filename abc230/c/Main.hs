{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Function
import Data.Int
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Void
import Data.Word
import Debug.Trace
import qualified Data.Array.Unboxed as UA
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

readTuples :: Int -> IO [(Int, Int)]
readTuples n = replicateM n (t <$> readIntList)
  where t [x, y] = (x, y)

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

printCMatrix :: CMatrix -> IO ()
printCMatrix m = mapM_ printRow rs
  where rs         = range . pair (fst . fst, fst . snd) $ bounds m
        cs         = range . pair (snd . fst, snd . snd) $ bounds m
        printRow r = mapM_ (putChar . (m!)) [(r, j) | j <- cs] >> putStrLn ""

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

new :: a -> b -> b
new _ x = x

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> CMatrix
solve n a b p q r s = accumArray new '.' ((p, r), (q, s))
                      $ [((a+k, b+k), '#') | k <- ks1] ++ [((a+k, b-k), '#') | k <- ks2]
  where ks1 = [max (p-a) (r-b)..min (q-a) (s-b)]
        ks2 = [max (p-a) (b-s)..min (q-a) (b-r)]

main :: IO ()
main = do
  [n, a, b] <- readIntList
  [p, q, r, s] <- readIntList
  printCMatrix $ solve n a b p q r s
