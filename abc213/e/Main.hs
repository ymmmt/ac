{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeFamilies #-}

-- import Control.Applicative ((<*>), Alternative, empty, (<|>))
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
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
import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

readCMatrix :: Int -> Int -> IO CMatrix
readCMatrix h w = listArray ((1, 1), (h, w)) . concat <$> replicateM h getLine

adjs0 :: CMatrix -> Cell -> [Cell]
adjs0 s (i, j) = [kl | kl <- [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
                     , inRange (bounds s) kl
                     , s!kl == '.']

adjs1 :: CMatrix -> Cell -> [Cell]
adjs1 s (i, j) = [(k, l) | di <- [-2..2], dj <- [-2..2]
                         , (di, dj) /= (0, 0)
                         , (abs di, abs dj) /= (2, 2)
                         , let k = i + di
                         , let l = j + dj
                         , inRange (bounds s) (k, l)]

-- https://atcoder.jp/contests/abc213/editorial/2397
solve :: Int -> Int -> CMatrix -> Int
solve h w s = runST $ do
  d <- newArray ((1, 1), (h, w)) maxBound :: ST s (STUArray s (Int, Int) Int)
  let go ((ij, c) Seq.:<| vcs)
        | ij == (h, w) = return c
        | otherwise    = do
            vcs0 <- map (,c) <$> (filterM (\kl -> (c<) <$> readArray d kl) $ adjs0 s ij)
            vcs1 <- map (,c+1) <$> (filterM (\kl -> (c+1<) <$> readArray d kl) $ adjs1 s ij)
            mapM_ (uncurry (writeArray d)) $ vcs0 ++ vcs1
            go $ foldl (Seq.|>) (foldr (Seq.<|) vcs vcs0) vcs1
  go $ Seq.singleton ((1, 1), 0)

main :: IO ()
main = do
  [h, w] <- readIntList
  s      <- readCMatrix h w
  print $ solve h w s
