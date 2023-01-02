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
import qualified Data.Graph as G
import qualified Data.Map as M
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
import qualified Data.Tree as T
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

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

numbers :: Int -> T.Tree G.Vertex -> M.Map G.Edge Int -> M.Map G.Vertex Int
numbers n (T.Node 1 ts) c = foldr (dfs 1 1) (M.singleton 1 1) ts
  where
    number k u v             = if k == num then diff num else num
      where num = c M.! (min u v, max u v)
    diff k                   = if k == n then 1 else k+1
    dfs k p (T.Node v ts) mp = foldr (dfs k' v) mp' ts
      where k'  = number k p v
            mp' = M.insert v k' mp

-- https://atcoder.jp/contests/arc108/editorial/348
solve :: Int -> [[Int]] -> [Int]
solve n uvcs = M.elems $ numbers n t c
  where
    g = buildUndirectedG (1, n) (map (\[u, v, _] -> (u, v)) uvcs)
    t = head $ G.dfs g [1]
    c = M.fromList [((min u v, max u v), c) | [u, v, c] <- uvcs]

main :: IO ()
main = do
  [n, m]  <- readIntList
  uvcs    <- readIntLists m
  putStrLns $ solve n uvcs
