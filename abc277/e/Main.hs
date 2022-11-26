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
import Control.Monad.ST
import Data.Array
import Data.Array.ST
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
import qualified Data.Vector.Unboxed as UV
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

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

edge :: [Int] -> G.Edge
edge [u, v, a] = (u, v)

see :: (MArray a Bool m, Ix i) => a i Bool -> i -> m ()
see marr i = writeArray marr i True

notSeen :: (MArray a Bool m, Ix i) => a i Bool -> i -> m Bool
notSeen marr i = not <$> readArray marr i

-- seenE: 偶数回スイッチを押した状態で訪れたか
-- seenO: 奇数回
bfs :: Int -> G.Graph -> G.Graph -> UA.UArray Int Bool -> Maybe Int
bfs n adjE adjO switch = runST $ do
  seenE <- newArray (1, n) False :: ST s (STUArray s Int Bool)
  seenO <- newArray (1, n) False :: ST s (STUArray s Int Bool)
  writeArray seenE 1 True
  let go d es os
        | n `elem` es || n `elem` os = return (Just d)
        | null es && null os         = return Nothing
        | otherwise = do
            mapM_ (see seenE) es
            mapM_ (see seenO) os
            esE <- filterM (notSeen seenE) $ concatMap (adjE!) es
            esO <- filterM (notSeen seenE) . concatMap (adjE!) $ filter (switch UA.!) os
            osO <- filterM (notSeen seenO) $ concatMap (adjO!) os
            osE <- filterM (notSeen seenO) . concatMap (adjO!) $ filter (switch UA.!) es
            go (d+1) (esE ++ esO) (osO ++ osE)
  go 0 [1] []
  
solve :: Int -> Int -> Int -> [[Int]] -> [Int] -> Int
solve n m k xs ss = fromMaybe (-1) $ bfs n adjE adjO switch
  where
    adjE = buildUndirectedG (1, n) . map edge $ filter ((==1) . (!!2)) xs
    adjO = buildUndirectedG (1, n) . map edge $ filter ((==0) . (!!2)) xs
    switch = UA.accumArray (||) False (1, n) $ zip ss (repeat True) :: UA.UArray Int Bool
      
main :: IO ()
main = do
  [n, m, k] <- readIntList
  es        <- readIntLists m
  ss        <- readIntList
  print $ solve n m k es ss
