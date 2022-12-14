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
import Data.Foldable
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

dfsPath :: G.Graph -> G.Vertex -> G.Vertex -> Maybe [G.Vertex]
dfsPath g s t = go (-1) [s] 
  where
    go p vs@(v:_)
      | v == t    = Just (reverse vs)
      | null cs   = Nothing
      | otherwise = asum $ map (go v . (:vs)) cs
      where cs = delete p (g!v)

solve :: Int -> [G.Edge] -> String
solve n abs = if s1 > sn then "Fennec" else "Snuke"
  where
    t      = buildUndirectedG (1, n) abs
    p      = fromJust $ dfsPath t 1 n
    k      = ceiling $ fromIntegral (length p - 2) / 2.0
    (u, v) = (p!!k, p!!(k+1))
    t'     = buildUndirectedG (1, n) $ filter (`notElem` [(u, v), (v, u)]) abs
    tu     = head $ G.dfs t' [u]
    tv     = head $ G.dfs t' [v]
    s1     = length $ T.flatten tu
    sn     = length $ T.flatten tv

main :: IO ()
main = do
  n <- readInt
  abs <- readTuplesAll
  putStrLn $ solve n abs
