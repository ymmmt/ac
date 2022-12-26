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
-- {-# LANGUAGE TupleSections #-}
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
-- import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

readCMatrix :: Int -> Int -> IO CMatrix
readCMatrix h w = listArray ((1, 1), (h, w)) . concat <$> replicateM h getLine

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

type Partition a = [Segment a]
type Segment a   = [a]

splits :: [a] -> [(Segment a, Segment a)]
splits []     = []
splits (x:xs) = ([x], xs):[(x:ys, zs) | (ys, zs) <- splits xs]

parts :: [a] -> [Partition a]
parts [] = [[]]
parts xs = [ys:yss | (ys, zs) <- splits xs, yss <- parts zs]

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z []     = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

white :: Char -> Bool
white = (== '1')

inf :: Int
inf = maxBound

-- https://img.atcoder.jp/abc159/editorial.pdf
solve :: Int -> Int -> Int -> CMatrix -> Int
solve h w k s = minimum $ map cost (parts [1..h])
  where
    whites c rs = count white [s!(r, c) | r <- rs]
    cost rss    = fromMaybe inf $ fst <$> foldM' step (length rss - 1, repeat 0) [1..w] -- (length rss - 1): cost for row partition
      where
        step (x, ss) c
          | any (> k) ns  = Nothing
          | any (> k) ss' = Just (x+1, ns)
          | otherwise     = Just (x, ss')
          where
            ns  = map (whites c) rss
            ss' = zipWith (+) ss ns

main :: IO ()
main = do
  [h, w, k] <- readIntList
  s         <- readCMatrix h w
  print $ solve h w k s
