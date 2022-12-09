{-# OPTIONS_GHC -O2 #-}

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
-- import Debug.Trace
-- import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Tree as T
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

choices :: Int -> [a] -> [[a]]
choices 0 _   = [[]]
choices _ []  = []
choices n (x:xs)
  | n > 0     = (map (x:) $ choices (n-1) xs) ++ choices n xs
  | otherwise = []

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

clique :: G.Graph -> [G.Vertex] -> Bool
clique g vs = all adj $ choices 2 vs
  where adj [v, w] = v `elem` g!w

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

solve :: Int -> [(Int, Int)] -> Int
solve n xys = maximum . map length . filter (clique g) $ powerset [1..n]
  where g = buildUndirectedG (1, n) xys

main :: IO ()
main = do
  [n, m] <- readIntList
  xys    <- readTuplesAll
  print $ solve n xys
