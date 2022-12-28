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
-- import Debug.Trace
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

-- https://kazun-kyopro.hatenablog.com/entry/ABC/245/F
solve :: Int -> [G.Edge] -> Int
solve n es = if all (== 1) (elems sizes) then 0
             else let seen = S.fromList . map (nums!) $ filter ((>= 2) . (sizes!) . (nums!)) [1..n]
                  in sum . map (sizes!). S.toList $ foldr step seen [1..k]
  where
    step i seen     = if S.member i seen then (dfs [i] seen) else seen
    dfs [] seen     = seen
    dfs (v:vs) seen = dfs (vs' ++ vs) seen'
      where vs'     = filter (flip S.notMember seen) (g'!v)
            seen'   = foldr S.insert seen vs'
    g               = G.buildG (1, n) es
    cs              = map T.flatten $ G.scc g
    k               = length cs
    nums            = array (1, n) . concatMap (\(i, vs) -> map (,i) vs) $ zip [1..k] cs
    sizes           = listArray (1, k) $ map length cs
    g'              = G.buildG (1, k) [(nums!v, nums!u) | (u, v) <- es, nums!u /= nums!v] -- 強連結成分グラフの成分間の辺の方向を反転したグラフ

main :: IO ()
main = do
  [n, m] <- readIntList
  es     <- readTuples m
  print $ solve n es
