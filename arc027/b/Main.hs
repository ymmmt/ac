{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

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

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

sg :: Ord a => [a] -> [[a]]
sg = group . sort

sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . sg

solve :: Int -> String -> String -> Int
solve n s t = product (map cnt vss)
  where
    st  = s ++ t
    c   = listArray (0, 2*n-1) st
    is  = findIndices isAlpha st
    es  = [(i, j) | i <- is, j <- is, i < j, c!i == c!j] ++ [(i, i+n) | i <- [0..n-1]]
    g   = buildUndirectedG (0, 2*n-1) es
    vss = map T.flatten (G.components g)
    cnt vs
      | k > 1           = 0
      | k == 1 && p     = if d == '0' then 0 else 1
      | k == 1 && not p = 1
      | k == 0 && p     = 9
      | k == 0 && not p = 10
      where ds = sortUniq . filter isDigit $ map (c!) vs
            d  = head ds
            k  = length ds
            p  = 0 `elem` vs

main :: IO ()
main = do
  n <- readInt
  s <- getLine
  t <- getLine
  print $ solve n s t
