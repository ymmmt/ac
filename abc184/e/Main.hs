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
-- import Data.Array
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
-- import qualified Data.Graph as G
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

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

readCMatrix :: Int -> Int -> IO CMatrix
readCMatrix h w = do
  rs <- replicateM h getLine
  let as               = concatMap colAssocs $ zip [1..h] rs
      colAssocs (i, r) = map (cross ((i,), id)) $ zip [1..w] r
  return $ array ((1, 1), (h, w)) as

-- printCMatrix :: CMatrix -> IO ()
-- printCMatrix m = mapM_ printRow rs
--   where
--     ((x, y), (z, w)) = bounds m
--     rs               = range (x, z)
--     cs               = range (y, w)
--     printRow r       = mapM_ (putChar . (m!)) [(r, j) | j <- cs] >> putStrLn ""

adjacents :: CMatrix -> Cell -> [Cell]
adjacents c (i, j)
  | c!(i, j) == '#' = []
  | otherwise       = filter ((&&) <$> inRange (bounds c) <*> (/= '#') . (c!))
                      [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

-- encode :: Int -> Cell -> G.Vertex
-- encode w (i, j) = w * (i - 1) + j

-- edges :: CMatrix -> [(Cell, Cell)]
-- edges c = concatMap (\ij -> map (ij,) $ adjacents c ij) $ indices c

-- encodeEdges :: Int -> [(Cell, Cell)] -> [G.Edge]
-- encodeEdges w = map (cross (encode w, encode w))

-- buildG :: CMatrix -> G.Graph
-- buildG c = G.buildG (1, h*w) . encodeEdges w $ edges c
--   where ((1, 1), (h, w)) = bounds c

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

findDeepest :: G.Graph -> G.Vertex -> G.Vertex
findDeepest g r = snd . head . last . takeWhile (not . null)
                  $ iterate (concatMap children) [(-1, r)]
  where children (p, v) = map (v,) $ filter (/= p) (g!v)

type Depth = Int

bfs :: CMatrix -> Cell -> Cell -> Array Char [Cell] -> Depth
bfs a s g warps = fromJust . findIndex ((g `elem`) . snd)
                  . iterate (\(s, vs) -> foldr op (s, []) vs) (S.empty, [(-1, s)])
  where
    op (p, v) (s, us) = 

solve :: Int -> Int -> CMatrix -> Int
solve h w a = bfs g s t warps
  where 
    warps = accumArray (flip (:)) [] ('a', 'z')
            . map swap
            . filter (inRange ('a', 'z') . snd) $ assocs a
    s = fst . head . filter ((== 'S') . snd) $ assocs a
    g = fst . head . filter ((== 'G') . snd) $ assocs a
    
    
    
    

main :: IO ()
main = do
  [h, w] <- readIntList
  a      <- readCMatrix h w
  print $ solve h w a
