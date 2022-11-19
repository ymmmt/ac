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
import qualified Data.Tree as T
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

type Distance = Int

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

-- https://atcoder.jp/contests/abc267/editorial/4714
solve :: Int -> [G.Edge] -> [(G.Vertex, Distance)] -> [G.Vertex]
solve n abs uks = map ans uks
  where 
    g                = buildUndirectedG (1, n) abs
    t                = head $ G.dfs g [1]
    l                = head . last $ T.levels t
    t'               = head $ G.dfs g [l]
    r                = head . last $ T.levels t'
    lsL              = T.levels . head $ G.dfs g [l]
    lsR              = T.levels . head $ G.dfs g [r]
    vsL, vsR         :: Map.Map Distance [G.Vertex]
    vsL              = Map.fromList $ zip [0..] lsL
    vsR              = Map.fromList $ zip [0..] lsR
    dsL, dsR         :: Array G.Vertex Distance
    dsL              = array (1, n) . concatMap (\(d, vs) -> map (,d) vs) $ zip [0..] lsL
    dsR              = array (1, n) . concatMap (\(d, vs) -> map (,d) vs) $ zip [0..] lsR
    ans (u, k)       = fromMaybe (-1) $ lookupL u k <|> lookupR u k
    lookup vs ds u k = (head <$> Map.lookup (ds!u - k) vs) <|>
                       (head <$> Map.lookup (ds!u + k) vs)
    lookupL          = lookup vsL dsL
    lookupR          = lookup vsR dsR

main :: IO ()
main = do
  n   <- readInt
  abs <- readTuples (n-1)
  q   <- readInt
  uks <- readTuples q
  putStr . unlines . map show $ solve n abs uks
