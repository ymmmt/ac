{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE BangPatterns #-}
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
-- import Data.Function
-- import Data.Int
-- import Data.Ix
import Data.List
import Data.Maybe
-- import Data.Tuple
-- import Data.Void
-- import Data.Word
-- import Debug.Trace
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
import qualified Data.Set as S
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
{-# INLINE swap #-}

see :: (MArray a Bool m, Ix i) => a i Bool -> i -> m ()
see marr i = writeArray marr i True
{-# INLINE see #-}

notSeen :: (MArray a Bool m, Ix i) => a i Bool -> i -> m Bool
notSeen marr i = not <$> readArray marr i
{-# INLINE notSeen #-}

warp :: Char -> Bool
warp = inRange ('a', 'z')
{-# INLINE warp #-}

adjs :: Int -> Int -> UA.UArray Int Char -> Int -> [Int]
adjs h w l u
  | l UA.! u == '#' = []
  | otherwise       = f (u - w >= 1) (u - w) ++
                      f (u + w <= h*w) (u + w) ++
                      f (w > 1 && u `mod` w /= 1) (u - 1) ++
                      f (w > 1 && u `mod` w /= 0) (u + 1)
  where f p v = if p then [v] else []
               
solve :: Int -> Int -> BS.ByteString -> Int
solve h w a = bfs
  where
    a'    = BS.unpack a
    warps = accumArray (flip (:)) [] ('a', 'z')
            . filter (warp . fst) $ zip a' [1..]
    l     = UA.listArray (1, h*w) a' :: UA.UArray Int Char
    s     = 1 + (fromJust $ BS.elemIndex 'S' a)
    t     = 1 + (fromJust $ BS.elemIndex 'G' a)
    bfs   = runST $ do
      seen <- newArray (1, h*w) False :: ST s (STUArray s Int Bool)
      wa   <- newArray ('#', 'z') False :: ST s (STUArray s Char Bool)
      see seen s
      let go !d vs
            | t `elem` vs = return d
            | null vs     = return (-1)
            | otherwise   = do
                let step vs v = do
                      b   <- (warp (l UA.! v) &&) <$> notSeen wa (l UA.! v)
                      vs' <- filterM (notSeen seen) $ if b then adjs h w l v ++ warps!(l UA.! v) else adjs h w l v
                      when b $ see wa (l UA.! v)
                      mapM_ (see seen) vs'
                      return $ vs' ++ vs
                vs' <- foldM step [] vs
                go (d+1) vs'
      go 0 [s]

main :: IO ()
main = do
  [h, w] <- readIntList
  a      <- BS.concat . BS.lines <$> BS.getContents
  print $ solve h w a
