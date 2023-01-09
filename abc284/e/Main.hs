{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE BangPatterns #-}
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
-- import qualified Data.Map as M
-- import qualified Data.Ratio as R
import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

tup :: [a] -> (a, a)
tup [x, y] = (x, y)

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

readTuples :: Int -> IO [(Int, Int)]
readTuples n = replicateM n (tup <$> readIntList)

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

solve :: Int -> [G.Edge] -> Int
solve n es = min k $ dfs 0 [(S.singleton 1, 1)]
  where
    k             = 10^6
    g             = buildUndirectedG (1, n) es
    dfs !acc []   = acc
    dfs !acc ((s, v):svs)
      | acc > k   = k
      | otherwise = dfs (acc+1) (svs' ++ svs)
      where
        svs'      = map (visit s) $ filter (flip S.notMember s) (g!v)
    visit seen v  = (S.insert v seen, v)

main :: IO ()
main = do
  [n, m] <- readIntList
  es     <- readTuples m
  print $ solve n es
