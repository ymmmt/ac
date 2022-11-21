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
import Data.Array
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
import qualified Data.Tree as T
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Numeric as N

-- Data.Function.Memoize

class Memoizable a where
  memoize :: (a -> v) -> a -> v

memoize2 :: (Memoizable a, Memoizable b) =>
            (a -> b -> v) -> a -> b -> v
memoize2 v = memoize (memoize . v)

memoize3 :: (Memoizable a, Memoizable b, Memoizable c) =>
            (a -> b -> c -> v) -> a -> b -> c -> v
memoize3 v = memoize (memoize2 . v)

memoize4 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d) =>
            (a -> b -> c -> d -> v) -> a -> b -> c -> d -> v
memoize4 v = memoize (memoize3 . v)

memoize5 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e) =>
            (a -> b -> c -> d -> e -> v) -> a -> b -> c -> d -> e -> v
memoize5 v = memoize (memoize4 . v)

memoize6 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e, Memoizable f) =>
            (a -> b -> c -> d -> e -> f -> v) ->
            a -> b -> c -> d -> e -> f -> v
memoize6 v = memoize (memoize5 . v)

memoize7 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e, Memoizable f, Memoizable g) =>
            (a -> b -> c -> d -> e -> f -> g -> v) ->
            a -> b -> c -> d -> e -> f -> g -> v
memoize7 v = memoize (memoize6 . v)

memoFix :: Memoizable a => ((a -> v) -> a -> v) -> a -> v
memoFix ff = f where f = memoize (ff f)

memoFix2 :: (Memoizable a, Memoizable b) =>
            ((a -> b -> v) -> a -> b -> v) -> a -> b -> v
memoFix2 ff = f where f = memoize2 (ff f)

memoFix3 :: (Memoizable a, Memoizable b, Memoizable c) =>
            ((a -> b -> c -> v) -> a -> b -> c -> v) -> a -> b -> c -> v
memoFix3 ff = f where f = memoize3 (ff f)

memoFix4 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d) =>
            ((a -> b -> c -> d -> v) -> (a -> b -> c -> d -> v)) ->
            a -> b -> c -> d -> v
memoFix4 ff = f where f = memoize4 (ff f)

memoFix5 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e) =>
            ((a -> b -> c -> d -> e -> v) -> (a -> b -> c -> d -> e -> v)) ->
            a -> b -> c -> d -> e -> v
memoFix5 ff = f where f = memoize5 (ff f)

memoFix6 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e, Memoizable f) =>
            ((a -> b -> c -> d -> e -> f -> v) -> (a -> b -> c -> d -> e -> f -> v)) ->
            a -> b -> c -> d -> e -> f -> v
memoFix6 ff = f where f = memoize6 (ff f)

memoFix7 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
             Memoizable e, Memoizable f, Memoizable g) =>
            ((a -> b -> c -> d -> e -> f -> g -> v) -> (a -> b -> c -> d -> e -> f -> g -> v)) ->
            a -> b -> c -> d -> e -> f -> g -> v
memoFix7 ff = f where f = memoize7 (ff f)

traceMemoize :: (Memoizable a, Show a) => (a -> b) -> a -> b
traceMemoize f = memoize (\a -> traceShow a (f a))

data BinaryTreeCache v = BinaryTreeCache {
  btValue         :: v,
  btLeft, btRight :: BinaryTreeCache v
  }
  deriving Functor

newtype Finite a = ToFinite { fromFinite :: a }
  deriving (Eq, Bounded, Enum)

instance (Bounded a, Enum a) => Memoizable (Finite a) where
  memoize f = finiteLookup (f <$> theFinites)

theFinites :: (Bounded a, Enum a) => BinaryTreeCache a
theFinites = loop minBound maxBound
  where loop start stop = BinaryTreeCache {
          btValue = mean,
          btLeft  = loop start (pred mean),
          btRight = loop (succ mean) stop
          }
          where mean = meanFinite start stop

finiteLookup :: (Bounded a, Enum a) => BinaryTreeCache v -> a -> v
finiteLookup cache0 a0 = loop start0 stop0 cache0
  where start0 = fromEnum (minBound `asTypeOf` a0)
        stop0  = fromEnum (maxBound `asTypeOf` a0)
        a      = fromEnum a0
        loop start stop cache =
          let mean = meanFinite start stop in
            case a `compare` mean of
              EQ -> btValue cache
              LT -> loop start (pred mean) (btLeft cache)
              GT -> loop (succ mean) stop (btRight cache)

meanFinite :: (Bounded a, Enum a) => a -> a -> a
meanFinite a b = toEnum (ia `div` 2 + ib `div` 2 +
                          if odd ia && odd ib then 1 else 0)
  where ia = fromEnum a
        ib = fromEnum b

memoizeFinite :: (Enum a, Bounded a) => (a -> v) -> a -> v
memoizeFinite f = memoize (f . fromFinite) . ToFinite

instance Memoizable Int    where memoize = memoizeFinite
instance Memoizable Char   where memoize = memoizeFinite
instance Memoizable Word   where memoize = memoizeFinite
instance Memoizable Word8  where memoize = memoizeFinite
instance Memoizable Word16 where memoize = memoizeFinite
instance Memoizable Word32 where memoize = memoizeFinite
instance Memoizable Word64 where memoize = memoizeFinite

-- deriveMemoizable ''()
-- deriveMemoizable ''Bool
-- deriveMemoizable ''Ordering
-- deriveMemoizable ''Maybe
-- deriveMemoizable ''Either
-- deriveMemoizable ''[]
-- deriveMemoizable ''Complex.Complex
-- deriveMemoizable ''Version.Version

-- deriveMemoizable ''Tuple.Solo

-- deriveMemoizable ''(,)
-- deriveMemoizable ''(,,)
-- deriveMemoizable ''(,,,)
-- deriveMemoizable ''(,,,,)
-- deriveMemoizable ''(,,,,,)
-- deriveMemoizable ''(,,,,,,)
-- deriveMemoizable ''(,,,,,,,)
-- deriveMemoizable ''(,,,,,,,,)
-- deriveMemoizable ''(,,,,,,,,,)
-- deriveMemoizable ''(,,,,,,,,,,)
-- deriveMemoizable ''(,,,,,,,,,,,)

encodeInteger :: Integer -> [Int]
encodeInteger 0 = []
encodeInteger i | minInt <= i && i <= maxInt
                = [fromInteger i]
encodeInteger i = fromInteger (i .&. maxInt) : encodeInteger (i `shiftR` intBits)

decodeInteger :: [Int] -> Integer
decodeInteger = foldr op 0
  where op i i' = fromIntegral i .|. i' `shiftL` intBits

intBits :: Int
intBits = finiteBitSize (0 :: Int) - 1

minInt, maxInt :: Integer
minInt = fromIntegral (minBound :: Int)
maxInt = fromIntegral (maxBound :: Int)

instance (Eq a, Bounded a, Enum a, Memoizable b) => Memoizable (a -> b) where
  memoize = functionLookup . theFunctions

functionLookup :: (Eq a, Bounded a, Enum a, Memoizable b) =>
                  FunctionCache b v -> (a -> b) -> v
functionLookup cache f =
  fcNil (foldl fcCons cache (f <$> [minBound .. maxBound]))

theFunctions :: (Eq a, Bounded a, Enum a, Memoizable b) =>
                ((a -> b) -> v) -> FunctionCache b v
theFunctions f = FunctionCache {
  fcNil  = f undefined,
  fcCons = memoize (\b -> theFunctions (f . extend b))
  }
  where extend b g a | a == minBound = b
                     | otherwise     = g (pred a)

data FunctionCache b v
  = FunctionCache {
      fcNil  :: v,
      fcCons :: b -> FunctionCache b v
    }

--

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

readTuples :: Int -> IO [(Int, Int)]
readTuples n = replicateM n (t <$> readIntList)
  where t [x, y] = (x, y)

maximumOn :: Ord a => (b -> a) -> [b] -> (b, a, Int)
maximumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx >= ky then u else v

log2Floor :: Int -> Int
log2Floor x = floor $ logBase 2 x

type Size     = Int
type Depth    = Int
type Height   = Int
type Distance = Int

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

graphSize :: G.Graph -> Size
graphSize = rangeSize (bounds g)

-- treeEdges :: T.Tree G.Vertex -> [G.Edge]
-- treeEdges (T.Node _ []) = []
-- treeEdges (T.Node v ts) = map ((,v) . T.rootLabel) ts ++ concatMap treeEdges ts

-- parent :: G.Graph -> G.Vertex -> Array G.Vertex G.Vertex
-- parent g r = array (bounds g) $ treeEdges t
--   where t = head $ G.dfs g [r]

-- Micro nodes

-- type TreeShape = [Bool]

-- valid :: TreeShape -> Bool
-- valid [] = True
-- valid bs = all (>= 0) cs && last cs == 0
--   where cs          = scanl1 count bs
--         count acc b = acc + (if b then 1 else (-1))

-- treeShapes :: Size -> [TreeShape]
-- treeShapes n
--   | n <= 1    = [[]]
--   | otherwise = concatMap f [1..n-1]
--   where f k = do
--           s  <- treeShapes k
--           s' <- treeShapes (n-k)
--           return $ (True:s) ++ (False:s')

-- -- treeShapes :: Size -> [TreeShape]
-- -- treeShapes = memoFix $ \shapes n ->
-- --   let f k = do
-- --         s <- shapes k
-- --         t <- shapes (n-k)
-- --         return $ (True:s) ++ (False:t)
-- --   in if n <= 1 then [[]]
-- --      else concatMap f [1..n-1]

-- treeShape :: T.Tree a -> TreeShape
-- treeShape (T.Node _ ts) = foldr step [] ts
--   where step t bs = True:(treeShape t) ++ (False:bs)

-- encode :: TreeShape -> Int
-- encode = foldl' step 0
--   where step acc b = acc * 10 + fromEnum b

-- decode :: Int -> Maybe TreeShape
-- decode x = mapM bool $ showIntAtBase 2 intToDigit x ""
--   where bool '1' = Just True
--         bool '0' = Just False
--         bool _   = Nothing

-- Macro nodes

data Tree a = Node { rootLabel :: a
                   , subForest :: [Tree a]
                   , parent    :: Maybe (Tree a)
                   , size      :: Size
                   , depth     :: Depth
                   , height    :: Height }

instance Eq a => Eq (Tree a) where
  (==) = (==) `on` rootLabel

type LongPath a = [Tree a]
type Ladder     = Array Depth G.Vertex

rebuild :: T.Tree a -> Tree a
rebuild = go 0 Nothing
  where
    go d p (T.Node v ts) = n
      where ts' = map (go (d+1) (Just n)) ts
            s   = 1 + (sum $ map size ts')
            h   = 1 + (maximum $ map height ts')
            n   = Node v ts' p s d h

-- -- Data.Tree.flatten
-- nodes :: Tree a -> [Tree a]
-- nodes t = squish t []
--   where squish t ns = t:foldr squish ns (subForest t)

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go
  where go (Node v ts _ _ _ _) = f v (map go ts)

dfs :: G.Graph -> G.Vertex -> Tree G.Vertex
dfs g r = rebuild . head $ G.dfs g [r]

-- Long-path decomposition
lpd :: Eq a => Tree a -> [LongPath a]
lpd t | null $ subForest t = [t]
lpd t@(Node v ts _ _ d h) = ((t:l):ls) ++ concatMap lpd ts'
  where (t', _, _) = maximumOn height ts
        ts'        = delete t' ts
        (l:ls)     = lpd t'

extend   :: LongPath G.Vertex -> ([G.Vertex], Ladder)
extend l = (vs, listArray (d, d+len-1) . map rootLabel $ ps ++ l)
  where
    top   = head l
    par t = (\p -> (p, p)) <$> parent t
    ps    = take (height top) $ unfoldr par top
    d     = depth top - length ps
    len   = depth top - d + height top

-- Ladder decomposition
ld :: Tree G.Vertex -> G.Bounds -> Array G.Vertex Ladder
ld t b = array b . concatMap (\(vs, l) -> map (,l) vs) . map extend $ lpd t

findDepth :: Ladder -> Depth -> Maybe G.Vertex
findDepth l d
  | inRange (bounds l) d = Just (l!d)
  | otherwise            = Nothing

microTreeSizeMax :: Size -> Size
microTreeSizeMax n = max 1 . floor $ (logBase 2 n) / 4

data LANode = MacroNode { depth    :: Depth
                        , jumpNode :: LANode }
            | JumpNode  { depth    :: Depth
                        , jumps    :: Array Int G.Vertex }
            | MicroNode { parent   :: G.Vertex
                        , depth    :: Depth
                        , jumpNode :: LANode }

jumps :: Tree G.Vertex -> Array G.Vertex Ladder -> Array Int G.Vertex
jumps t lad
  | depth t == 0 = error "root node is jump node"
  | otherwise = listArray (0, n-1) vs
  where n = length vs
        d = depth t - 1
        v = fromJust $ parent t
        vs = v:jumps' 1 (lad!v)
        jumps' :: Distance -> Ladder -> [G.Vertex]
        jumps' k l
          | d - k < 0 = []
          | inRange (bounds l) (d - k) = (l!(d - k)):jumps' (2*k) l
          | otherwise = jumps' k $ lad!(l!d0)
          where d0 = fst $ bounds l

laNodes :: Tree G.Vertex -> G.Bounds -> Array G.Vertex LANode
laNodes t b l = array b $ macros t
  where thr = microTreeSizeMax n
        n = rangeSize b
        macros :: Tree G.Vertex -> [(G.Vertex, LANodes)]
        macros t@(Node v [] _ _ d _) = [(v, JumpNode d $ jumps t l)]
        macros t@(Node v ts _ _ d _)
          | h > thr   = let j    = case snd x of
                                     (MacroNode _ j')  -> j'
                                     j'@(JumpNode _ _) -> j'
                                     (MicroNode _ _)   -> error "unexpected micro node"
                            x:xs = concatMap macros ts
                        in (v, MacroNode d j):x:xs
          | otherwise = let j = JumpNode d (jumps t l)
                        in (v, j):concatMap (micros j) ts
        micros :: LANode -> Tree G.Vertex -> [(G.Vertex, LANodes)]
        micros j (Node v ts (Just p) _ d _) = (v, MicroNode (rootLabel p) d j):concatMap (micros j) ts

nthParent :: LANode -> Array G.Vertex LANode -> Distance -> G.Vertex
nthParent m@(MicroNode p _ _) _ 1 = p
nthParent m@(MicroNode p _ _) n k = nthParent (n!p) n (k - 1)

levelAncestor :: G.Graph -> G.Vertex -> (G.Vertex -> Depth -> Maybe G.Vertex)
levelAncestor g r = ans
  where
    t = dfs g r
    l = ld t $ bounds g
    n = laNodes t (bounds g)
    thr = microNodeHeightThreshold $ graphSize g
    ans u k = case n!u of
      MacroNode d (JumpNode d' js)
        | d - k < 0 -> Nothing
        | otherwise -> let i = log2Floor $ d' - (d - k)
                           v = js!i
                       in findDepth (l!v) (d - k)
      JumpNode d js -> let i = log2Floor k
                           v = js!i
                       in findDepth (l!v) (d - k)
      MicroNode p d (JumpNode d' js)
        | d - k < 0 -> Nothing
        | d - k >= d' -> Just $ nthParent m k
        | otherwise -> let i = log2Floor $ d' - (d - k)
                           v = js!i
                       in findDepth (l!v) (d - k)
    

-- levelAncestor :: G.Graph -> G.Vertex -> (G.Vertex -> Depth -> Maybe G.Vertex)
-- levelAncestor g r = la
--   where
--     t    :: T.Tree G.Vertex
--     t    = head $ G.dfs g [r]
--     p    :: Array G.Vertex G.Vertex
--     p    = array (bounds g) $ treeEdges t
--     ls   :: [[G.Vertex]]
--     ls   = T.levels t
--     ds   :: Array G.Vertex Depth
--     ds   = array (bounds g) . concatMap (\(d, vs) -> map (,d) vs) $ zip [0..] ls
--     jump :: G.Vertex -> Int -> G.Vertex
--     jump = memoFix2 (\f v i -> if i == 0 then p!v
--                                else f (f v (i-1)) (i-1))
--     la v d
--       | ds!v < d  = Nothing
--       | ds!v == d = Just v
--       | otherwise = la (jump v i) d
--       where i = log2Floor $ fromIntegral (ds!v - d)

-- levelAncestor :: G.Graph -> G.Vertex -> (G.Vertex -> Distance -> Maybe G.Vertex)
-- levelAncestor g r = ans
--   where
--     t      :: T.Tree G.Vertex
--     t      = head $ G.dfs g [r]
--     parent :: Array G.Vertex G.Vertex
--     parent = array (bounds g) $ treeEdges t
--     ls     :: [[G.Vertex]]
--     ls     = T.levels t
--     depth  :: Array G.Vertex Depth
--     depth  = array (bounds g) . concatMap (\(d, vs) -> map (,d) vs) $ zip [0..] ls
--     jump   :: G.Vertex -> Int -> G.Vertex
--     jump   = memoFix2 (\f v i -> if i == 0 then parent!v
--                                  else f (f v (i-1)) (i-1))
--     la v d
--       | dv < d  = Nothing
--       | dv == d = Just v
--       | otherwise = la (jump v i) d
--       where dv = depth!v
--             i  = log2Floor $ fromIntegral (dv - d)
--     ans u k
--       | du < k = Nothing
--       | du == k = Just r
--       | otherwise = la u (du - k)
--       where du = depth!u

-- https://atcoder.jp/contests/abc267/editorial/4714
solve :: Size -> [G.Edge] -> [(G.Vertex, Depth)] -> [G.Vertex]
solve n abs uks = map ans uks
  where 
    g                = buildUndirectedG (1, n) abs
    t                = head $ G.dfs g [1]
    l                = head . last $ T.levels t
    t'               = head $ G.dfs g [l]
    r                = head . last $ T.levels t'
    ansL              = levelAncestor g l
    ansR              = levelAncestor g r

    
    -- lsL              = T.levels . head $ G.dfs g [l]
    -- lsR              = T.levels . head $ G.dfs g [r]
    -- vsL, vsR         :: Map.Map Depth [G.Vertex]
    -- vsL              = Map.fromList $ zip [0..] lsL
    -- vsR              = Map.fromList $ zip [0..] lsR
    -- dsL, dsR         :: Array G.Vertex Depth
    -- dsL              = array (1, n) . concatMap (\(d, vs) -> map (,d) vs) $ zip [0..] lsL
    -- dsR              = array (1, n) . concatMap (\(d, vs) -> map (,d) vs) $ zip [0..] lsR
    ans (u, k)       = fromMaybe (-1) $ ansL u k <|> ansR u k

main :: IO ()
main = do
  n   <- readInt
  abs <- readTuples (n-1)
  q   <- readInt
  uks <- readTuples q
  putStr . unlines . map show $ solve n abs uks
