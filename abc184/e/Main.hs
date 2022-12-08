{-# OPTIONS_GHC -O2 #-}

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
import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
import qualified Data.Set as S
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

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

readCMatrix :: Int -> Int -> IO CMatrix
readCMatrix h w = do
  rs <- replicateM h getLine
  let as               = concatMap colAssocs $ zip [1..h] rs
      colAssocs (i, r) = map (cross ((i,), id)) $ zip [1..w] r
  return $ array ((1, 1), (h, w)) as

encode :: Int -> Cell -> G.Vertex
encode w (i, j) = w * (i - 1) + j

buildG :: Int -> CMatrix -> G.Graph
buildG w c = buildUndirectedG (1, h*w)
             . concatMap (\ij -> map ((encode w ij,) . encode w) $ adjs c ij)
             . map fst . filter ((/= '#') . snd) $ assocs c
  where ((1, 1), (h, w)) = bounds c
        adjs c (i, j)    = filter ((&&) <$> inRange (bounds c) <*> (/= '#') . (c!))
                           [(i+1, j), (i, j+1)]

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

see :: (MArray a Bool m, Ix i) => a i Bool -> i -> m ()
see marr i = writeArray marr i True

notSeen :: (MArray a Bool m, Ix i) => a i Bool -> i -> m Bool
notSeen marr i = not <$> readArray marr i

warp :: Char -> Bool
warp = inRange ('a', 'z')

solve :: Int -> Int -> CMatrix -> Int
solve h w a = bfs
  where
    g     = buildG w a
    warps = accumArray (flip (:)) [] ('a', 'z')
            . map (cross (id, encode w) . swap)
            . filter (warp . snd) $ assocs a
    l     = UA.listArray (1, h*w) $ elems a :: UA.UArray Int Char
    s     = encode w . fst . head . filter ((== 'S') . snd) $ assocs a
    t     = encode w . fst . head . filter ((== 'G') . snd) $ assocs a
    bfs   = runST $ do
      seen <- newArray (1, h*w) False :: ST s (STUArray s Int Bool)
      see seen s
      let go d w vs
            | t `elem` vs = return d
            | null vs     = return (-1)
            | otherwise   = do
                let step (w, vs) v = do
                      let b  = warp (l UA.! v) && S.notMember (l UA.! v) w
                          w' = if b then S.insert (l UA.! v) w else w
                      vs' <- filterM (notSeen seen) $ if b then g!v ++ warps!(l UA.! v) else g!v
                      mapM_ (see seen) vs'
                      return (w', vs' ++ vs)
                (w', vs') <- foldM step (w, []) vs
                go (d+1) w' vs'
      go 0 S.empty [s]

main :: IO ()
main = do
  [h, w] <- readIntList
  a      <- readCMatrix h w
  print $ solve h w a
