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
import Data.Complex
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

intLength :: Int -> Int
intLength n
  | n >= 0    = length $ N.showIntAtBase 2 intToDigit n ""
  | otherwise = undefined

interleave :: [a] -> ([a], [a])
interleave []         = ([], [])
interleave xs@[x]     = (xs, [])
interleave (x:y:rest) = (x:xs, y:ys)
  where (xs, ys) = interleave rest

single :: [a] -> Bool
single [x] = True
single _   = False

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

-- CoeffPoly: Coefficient Representation of Polynomials

type Degree  = Int
type Coeff   = Int
type CoeffPoly = Array Degree Coeff

unit :: CoeffPoly
unit = array (0, 0) [(0, 1)]

one :: CoeffPoly
one = array (1, 1) [(1, 1)]

unitOne :: CoeffPoly
unitOne = array (0, 1) [(0, 1), (1, 1)]

degC :: CoeffPoly -> Degree
degC = snd . bounds

addC :: CoeffPoly -> CoeffPoly -> CoeffPoly
addC f g = accumArray addMod 0 (0, max (degC f) (degC g)) $ assocs f ++ assocs g

naiveConvolve :: CoeffPoly -> CoeffPoly -> CoeffPoly
naiveConvolve f g =
  accumArray addMod 0 (0, degC f + degC g)
  [(i+j, ci*cj `mod` modulo) | (i, ci) <- assocs f, (j, cj) <- assocs g]

convolve :: CoeffPoly -> CoeffPoly -> CoeffPoly
convolve f g = ifft $ mulP (fft' f) (fft' g)
  where k = intLength $ max (degC f) (degC g)
        n = 2^k
        fft' f = fftRec n (zeroPad n f) False

mulC :: CoeffPoly -> CoeffPoly -> CoeffPoly
mulC f g
  | max (degC g) (degC g) <= 60 = naiveConvolve f g
  | otherwise                   = convolve f g

shiftR :: Degree -> CoeffPoly -> CoeffPoly
shiftR d = mulC (array (d, d) [(d, 1)])

zeroPad :: Int -> CoeffPoly -> [Value]
zeroPad n f = map (coeffToValue . ref) [0..n-1]
  where r     = bounds f
        ref i = if inRange r i then f!i else 0

-- https://medium.com/@aiswaryamathur/understanding-fast-fourier-transform-from-scratch-to-solve-polynomial-multiplication-8018d511162f
-- https://faculty.sites.iastate.edu/jia/files/inline-files/polymultiply.pdf
fft :: CoeffPoly -> PVPoly
fft f = fftRec (2^k) vs False
  where k = intLength $ degC f
        vs = zeroPad (2^k) f

fftRec :: Int -> [Value] -> Bool -> PVPoly
fftRec n vs ifftFlag
  | n == 1    = vs
  | n > 1     = zipWith3 f (cycle evRec) (cycle odRec) (take n $ iterate (*wn) 1)
  | otherwise = undefined
  where wn       = cis $ 2 * N.pi / (fromIntegral n) * (if ifftFlag then -1 else 1) :: Complex Float
        (ev, od) = interleave vs
        evRec    = fftRec (n `div` 2) ev ifftFlag
        odRec    = fftRec (n `div` 2) od ifftFlag
        f e o w  = e + w * o

ifft :: PVPoly -> CoeffPoly
ifft vs = listArray (0, n-1) $ map (valueToCoeff n) $ fftRec n vs True
  where n = length vs

-- PVPoly: Point-Value representaion of Polynomials

type Value  = Complex Float
type PVPoly = [Value]

valueToCoeff :: Int -> Value -> Coeff
valueToCoeff n = round . (/ fromIntegral n) . realPart

coeffToValue :: Coeff -> Value
coeffToValue = fromIntegral

degP :: PVPoly -> Degree
degP = length

-- addP :: PVPoly -> PVPoly -> PVPoly
-- addP f g
--   | degP f == degP g = zipWith (+) f g
--   | otherwise        = undefined

mulP :: PVPoly -> PVPoly -> PVPoly
mulP f g
  | degP f == degP g = zipWith (*) f g
  | otherwise        = undefined

-- Body

genF0 :: Tree -> CoeffPoly
genF0 (Leaf _) = unitOne
genF0 t        = addC m (shiftR 1 $ foldl1' addC ms')
  where gs  = map genG $ heavyPath t
        ms  = scanl' mulC unit gs
        ms' = init ms
        m   = last ms

-- genF :: Tree -> CoeffPoly
-- genF (Leaf _) = unitOne
-- genF t        = mulC gn $ foldr op unitOne gs
--   where op g acc = addOne $ mulC g acc
--         addOne f = f // [(1, f!1 + 1)]
--         gn       = genG t
--         gs       = map genG . tail $ heavyPath t

genG :: Tree -> CoeffPoly
genG (Leaf _) = unit
genG t        = foldl' mulC unit $ map genF0 (light t)

-- https://atcoder.jp/contests/abc269/editorial/4838
solve n ps = map (a!) [1..n]
  where g  = makeGraph n $ zip ps [2..n]
        t  = makeTree g 1
        f1 = genF0 t
        a  = accumArray addMod 0 (0, n) $ assocs f1

main :: IO ()
main = do
  n  <- readInt
  ps <- readIntList
  putStr . unlines . map show $ solve n ps
