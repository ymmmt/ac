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
import Debug.Trace
-- import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

tup :: [a] -> (a, a)
tup [x, y] = (x, y)

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

readIntListsAll :: IO [[Int]]
readIntListsAll = map toIntList . BS.lines <$> BS.getContents

readTuplesAll :: IO [(Int, Int)]
readTuplesAll = map tup <$> readIntListsAll

single :: [a] -> Bool
single [x] = True
single _   = False

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | x:ys <- tails xs, y:zs <- tails ys]

c2 :: Int -> Int
c2 n = if n >= 2 then n*(n-1)`div`2 else 0

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

bipartite :: G.Graph -> G.Vertex -> Maybe ([G.Vertex], [G.Vertex])
bipartite g s = dfs (S.singleton s) (S.empty) [(s, True)]
  where
    dfs e o []     = Just (S.toList e, S.toList o)
    dfs e o ((v, even):ves) = dfs e' o' . (++ ves) . map (, odd) =<< vs'
      where
        odd = not even
        e'  = if odd  then foldr S.insert e (fromJust vs') else e
        o'  = if even then foldr S.insert o (fromJust vs') else o
        vs' = foldr new (Just []) (g!v)
        new _ Nothing = Nothing
        new w m@(Just ws)
          | (even && S.member w e) || (odd && S.member w o) = Nothing
          | (even && S.member w o) || (odd && S.member w e) = m
          | otherwise                                       = Just (w:ws)

-- https://atcoder.jp/contests/abc282/editorial/5397
solve :: Int -> [G.Edge] -> Int
solve n es = if all bip xs
             then c2 n - sum (map inBip xs) - length es
             else 0
  where
    g                   = buildUndirectedG (1, n) es
    bip                 = isJust
    xs                  = map (bipartite g . T.rootLabel) (G.components g)
    inBip (Just (e, o)) = c2 (length e) + c2 (length o)

main :: IO ()
main = do
  [n, m] <- readIntList
  uvs    <- readTuplesAll
  print $ solve n uvs
