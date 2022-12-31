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
import Data.List hiding (intersect)
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
-- import qualified Data.Map as M
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

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

dists :: G.Graph -> G.Vertex -> Array G.Vertex Int
dists g r = array (bounds g) [(v, d) | (vs, d) <- zip (T.levels t) [0..], v <- vs]
  where
    t = head (G.dfs g [r])

leaves :: G.Graph -> [G.Vertex]
leaves = map fst . filter ((== 1) . snd) . assocs . G.indegree

intersect :: Ord a => [a] -> [a] -> [a]
intersect xs [] = []
intersect [] ys = []
intersect xs@(x:xs') ys@(y:ys')
  | x < y     = intersect xs' ys
  | x > y     = intersect xs ys'
  | otherwise = x:intersect xs' ys'

maximumOn :: Ord b => (a -> b) -> [a] -> (a, b, Int)
maximumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx >= ky then u else v

solve :: Int -> Int -> Int -> [G.Edge] -> Int
solve n u v es = du!l + t (dv!l - du!l)
  where
    (l, _, _)   = maximumOn (dv!) ls
    g           = buildUndirectedG (1, n) es
    du          = dists g u
    dv          = dists g v
    reachable x = du!x < dv!x
    g'          = buildUndirectedG (1, n) [(x, y) | (x, y) <- es, reachable x, reachable y]
    ls          = if u `elem` ls then u:int else int
      where ls  = leaves g
            ls' = leaves g'
            int = intersect ls ls'
    t d         = max 0 (d - 1)

main :: IO ()
main = do
  [n, u, v] <- readIntList
  es        <- readTuplesAll
  print $ solve n u v es
