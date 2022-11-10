{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad
-- import Data.Bits
import Data.Array
import Data.Char
import Data.Function
import Data.Int
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Word
import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Numeric as N

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)

modulo :: Int
modulo = 998244353

addMod :: Int -> Int -> Int
addMod x y = (x+y) `mod` modulo

-- Graph

type Graph = Array Idx [Idx]
type Edge = (Idx, Idx)

makeGraph :: Int -> [Edge] -> Graph
makeGraph n = accumArray (flip (:)) [] (1, n)

-- Tree

type Idx = Int
type Size = Int
type Heavy = Tree
type Light = [Tree]

data Tree = Leaf Idx
          | Tree Idx Size Heavy Light
            deriving (Show)

instance Eq Tree where
  (==) = ((==) `on` idx)

size :: Tree -> Size
size (Leaf _)       = 1
size (Tree _ s _ _) = s

idx :: Tree -> Idx
idx (Leaf i)       = i
idx (Tree i _ _ _) = i

light :: Tree -> Light
light (Leaf _)        = []
light (Tree _ _ _ ls) = ls

makeTree :: Graph -> Idx -> Tree
makeTree g i
  | null (g!i) = Leaf i
  | otherwise  = Tree i s h ls
    where s  = sum (map size cs) + 1
          cs = map (makeTree g) (g!i)
          h  = maximumBy (compare `on` size) cs
          ls = delete h cs

heavyPath :: Tree -> [Tree]
heavyPath l@(Leaf _)         = [l]
heavyPath t@(Tree _ _ h _) = t:heavyPath h

-- GenFunc: generating function

type Degree  = Int
type Coeff   = Int
type GenFunc = Array Degree Coeff

unit :: GenFunc
unit = array (0, 0) [(0, 1)]

-- one :: GenFunc
-- one = array (1, 1) [(1, 1)]

unitOne :: GenFunc
unitOne = array (0, 1) [(0, 1), (1, 1)]

deg :: GenFunc -> Degree
deg = snd . bounds

naiveConvolution :: Degree -> GenFunc -> GenFunc -> GenFunc
naiveConvolution d f g =
  accumArray addMod 0 (0, d')
  [(i+j, ci*cj `mod` modulo) | (i, ci) <- assocs f, (j, cj) <- assocs g, i+j <= d']
  where d' = min d (deg f + deg g)

convolution :: Degree -> GenFunc -> GenFunc -> GenFunc
convolution d f g = undefined

-- shiftR :: Degree -> GenFunc -> GenFunc
-- shiftR d f = array (0, d') $ filter ((<= d') . fst) $ map (cross ((+1), id)) $ assocs f
--   where d' = min d (deg f + 1)

-- normal :: Degree -> GenFunc -> GenFunc
-- normal d = filter ((>0) . snd) . assocs . accumArray addMod 0 (0, d)

add :: GenFunc -> GenFunc -> GenFunc
add f g = accumArray addMod 0 (0, d') $ assocs f ++ assocs g
  where d' = max (deg f) (deg g)

mul :: Degree -> GenFunc -> GenFunc -> GenFunc
mul d f g
  | max (deg g) (deg g) <= 60 = naiveConvolution d f g
  | otherwise                 = convolution d f g
-- mul d f g = normal d
--                $ [(i+j, (ci*cj) `mod` modulo) | (i, ci) <- f, (j, cj) <- g, (i+j) <= d]

-- f :: Degree -> Tree -> GenFunc
-- f _ (Leaf _) = unitOne
-- f d t        = add d (shiftR d $ foldl1' (add d) ms') m
--   where gs  = map (g d) $ heavyPath t
--         ms  = scanl' (mul d) unit gs
--         ms' = init ms
--         m   = last ms

f :: Degree -> Tree -> GenFunc
f _ (Leaf _) = unitOne
f d t        = mul d gn $ foldr op unitOne gs
  where op g acc = addOne $ mul d g acc
        addOne f = f // [(1, f!1 + 1)]
        gn       = g d t
        gs       = map (g d) . tail $ heavyPath t

g :: Degree -> Tree -> GenFunc
g _ (Leaf _) = unit
g d t        = foldl' (mul d) unit $ map (f d) (light t)

-- https://atcoder.jp/contests/abc269/editorial/4838
solve n ps = map (a!) [1..n]
  where g  = makeGraph n $ zip ps [2..n]
        t  = makeTree g 1
        f1 = f n t
        a  = accumArray addMod 0 (0, n) $ assocs f1

main :: IO ()
main = do
  n  <- readInt
  ps <- readIntList
  putStr . unlines . map show $ solve n ps
