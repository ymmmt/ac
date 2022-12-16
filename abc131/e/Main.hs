{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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
-- import Debug.Trace
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

list :: (a, a) -> [a]
list (x, y) = [x, y]

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

joinStr :: Show a => String -> [a] -> String
joinStr sep xs = intercalate sep $ map show xs

fact :: Int -> Integer
fact n
  | n <= 1    = 1
  | otherwise = toInteger n * fact (n - 1)

choose :: Int -> Int -> Integer
choose n k
  | n < k || n < 0 || k < 0 = 0
  | otherwise               = fact n `div` fact k `div` fact (n - k)

-- https://blog.hamayanhamayan.com/entry/2019/06/22/225637
solve :: Int -> Int -> (Int, [(Int, Int)])
solve n k = if k > u then (-1, []) else (m, es)
  where
    u  = fromInteger $ choose (n-1) 2
    m  = (fromInteger $ choose n 2) - k
    es = map (1,) [2..n] ++ drop k [(i, j) | i <- [2..n-1], j <- [i+1..n]]

main :: IO ()
main = do
  [n, k] <- readIntList
  let (m, es) = solve n k
  print m
  putStr . unlines $ map (joinStr " " . list) es
