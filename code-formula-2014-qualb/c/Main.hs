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

import Control.Applicative ((<*>), Alternative, empty, (<|>))
import Control.Monad
-- import Control.Monad.ST
import Data.Array
-- import Data.Array.ST
import Data.Bits
import Data.Char
import Data.Function
import Data.Int
import Data.Ix
import Data.List
import Data.Maybe
-- import Data.Tuple
import Data.Void
import Data.Word
import Debug.Trace
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
import qualified Data.Map as Map
import qualified Data.Ratio as R
import qualified Data.Tree as T
import qualified Data.Set as S
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Numeric as N

tup :: [Int] -> (Int, Int)
tup [x, y] = (x, y)

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

yn :: Bool -> String
yn True  = "YES"
yn False = "NO"

bfilter :: [a] -> [Bool] -> [a]
bfilter xs bs = map fst . filter snd $ zip xs bs

choices :: Int -> [a] -> [[a]]
choices 0 _   = [[]]
choices _ []  = []
choices n (x:xs)
  | n > 0     = (map (x:) $ choices (n-1) xs) ++ choices n xs
  | otherwise = []

sg :: Ord a => [a] -> [[a]]
sg = group . sort

allUnique :: Ord a => [a] -> Bool
allUnique = all (null . tail) . sg

type Transposition = (Int, Int)

swap :: Transposition -> String -> String
swap (i, j) s
  | i == j    = s
  | otherwise = take k s ++ [s!!l] ++ drop (k+1) (take l s) ++ [s!!k] ++ drop (l+1) s
  where k = min i j
        l = max i j

swap3 :: String -> String -> Transposition -> Transposition -> Transposition -> Bool
swap3 as bs t1 t2 t3 = (swap t3 . swap t2 $ swap t1 as) == bs

match :: String -> String -> Bool -> Bool
match as bs dup = (n == 0 && dup)
                  || (n >= 2 && n <= 6 && (or $ (swap3 as bs) <$> ts' <*> ts' <*> ts'))
  where n   = length as
        ts  = map tup $ choices 2 [0..length as - 1]
        ts' = if dup then (0, 0):ts else ts

solve :: String -> String -> Bool
solve as bs = sort as == sort bs && match as' bs' dup
  where ds  = zipWith (/=) as bs
        as' = bfilter as ds
        bs' = bfilter bs ds
        dup = not $ allUnique as

main :: IO ()
main = do
  as <- getLine
  bs <- getLine
  putStrLn . yn $ solve as bs
