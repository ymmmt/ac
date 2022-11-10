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

-- GenFunc

type Degree = Int
type Coeff = Int
type GenFunc = [(Degree, Coeff)]

unit :: GenFunc
unit = [(0, 1)]

one :: GenFunc
one = [(1, 1)]

unitOne :: GenFunc
unitOne = [(0, 1), (1, 1)]

shiftR :: Int -> GenFunc -> GenFunc
shiftR maxdeg = mul maxdeg one
--shiftR maxdeg = normal maxdeg . map (cross ((1+), id))

normal :: Int -> GenFunc -> GenFunc
normal maxdeg = filter ((>0) . snd) . assocs . accumArray addMod 0 (0, maxdeg)

add :: Int -> GenFunc -> GenFunc -> GenFunc
add maxdeg f g = normal maxdeg (f ++ g)

mul :: Int -> GenFunc -> GenFunc -> GenFunc
mul maxdeg f g = normal maxdeg
               $ [(i+j, (c*d) `mod` modulo) | (i, c) <- f, (j, d) <- g, (i+j) <= maxdeg]

genF :: Int -> Tree -> GenFunc
genF _ (Leaf _) = unitOne
genF maxdeg t = add maxdeg (shiftR maxdeg $ foldl1' (add maxdeg) ms') m
  where gs  = map (genG maxdeg) $ heavyPath t
        ms  = scanl' (mul maxdeg) unit gs
        ms' = init ms
        m   = last ms
        
genG :: Int -> Tree -> GenFunc
genG _ (Leaf _) = unit
genG maxdeg t = foldl' (mul maxdeg) unit $ map (genF maxdeg) (light t)

-- https://atcoder.jp/contests/abc269/editorial/4838
solve n ps = map (a!) [1..n]
  where g = makeGraph n $ zip ps [2..n]
        t = makeTree g 1
        f = genF n t
        a = accumArray addMod 0 (0, n) f

main :: IO ()
main = do
  n  <- readInt
  ps <- readIntList
  putStr . unlines . map show $ solve n ps
