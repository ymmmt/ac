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
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Numeric as N

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
log2Floor x = floor . logBase 2 $ fromIntegral x

-- Graph

type Size     = Int
type Depth    = Int
type Height   = Int
type Distance = Int

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

graphSize :: G.Graph -> Size
graphSize g = rangeSize (bounds g)

-- Level Ancestor Problem
-- https://37zigen.com/level-ancestor-problem/
-- https://www.sciencedirect.com/science/article/pii/S0304397504001173

data Tree a = Node { rootLabel :: a
                   , subForest :: [Tree a]
                   , parent    :: Maybe (Tree a)
                   , size      :: Size
                   , depth     :: Depth
                   , height    :: Height }

instance Show a => Show (Tree a) where
  show t = "Node {rootLabel = " ++ (show $ rootLabel t)
           ++ ", subForest = " ++ show (map rootLabel $ subForest t)
           ++ ", parent = " ++ (show $ rootLabel <$> parent t)
           ++ ", size = " ++ (show $ size t)
           ++ ", depth = " ++ (show $ depth t)
           ++ ", height = " ++ (show $ height t) ++ "}"

instance Eq a => Eq (Tree a) where
  (==) = (==) `on` rootLabel

type LongPath a = [Tree a]
type Ladder     = Array Depth G.Vertex

data LANode = MacroNode { _depth    :: Depth
                        , jumpNode  :: LANode }
            | JumpNode  { _depth    :: Depth
                        , _jumps    :: Array Int G.Vertex }
            | MicroNode { _parent   :: G.Vertex
                        , _depth    :: Depth
                        , jumpNode  :: LANode }
            deriving (Show)

rebuild :: T.Tree a -> Tree a
rebuild = go 0 Nothing
  where
    go d p (T.Node v []) = Node v [] p 1 d 1
    go d p (T.Node v ts) = n
      where ts' = sortOn (negate . height) $ map (go (d+1) (Just n)) ts
            s   = 1 + (sum $ map size ts')
            h   = 1 + (height $ head ts')
            n   = Node v ts' p s d h

dfs :: G.Graph -> G.Vertex -> Tree G.Vertex
dfs g r = rebuild . head $ G.dfs g [r]

-- Long-path decomposition
lpd :: Eq a => Tree a -> [LongPath a]
lpd t = lpd' [t] [] []
  where
    lpd' [] [] xs = xs
    lpd' (t@(Node _ ts' _ _ _ _):ts) ps xs =
      if null ts'
      then (lpd' ts [] $ reverse (t:ps):xs)
      else lpd' (ts' ++ ts) (t:ps) xs

extend   :: LongPath G.Vertex -> ([G.Vertex], Ladder)
extend p = (vs, listArray (d, d+len-1) $ map rootLabel ts ++ vs)
  where
    vs    = map rootLabel p
    top   = head p
    par t = (\x -> (x, x)) <$> parent t
    ts    = reverse . take (height top) $ unfoldr par top
    d     = depth top - length ts
    len   = depth top - d + height top

-- Ladder decomposition
ld :: Tree G.Vertex -> G.Bounds -> Array G.Vertex Ladder
ld t b = array b . concatMap (\(vs, l) -> map (,l) vs) . map extend $ lpd t

microTreeSizeMax :: Size -> Size
microTreeSizeMax n = max 1 . floor $ (logBase 2 n') / 4
  where n' = fromIntegral n

jumps :: Tree G.Vertex -> Array G.Vertex Ladder -> Array Int G.Vertex
jumps t lad
  | depth t == 0 = error "root node is jump node"
  | otherwise    = listArray (0, n-1) vs
  where n  = length vs
        d  = depth t
        v  = rootLabel . fromJust $ parent t
        vs = jumps' 1 (lad!v)
        jumps' :: Distance -> Ladder -> [G.Vertex]
        jumps' k l
          | d - k < 0                  = []
          | inRange (bounds l) (d - k) = (l!(d - k)):jumps' (2*k) l
          | otherwise                  = jumps' k $ lad!(l!d0)
          where d0 = fst $ bounds l

jnFix :: LANode -> Tree G.Vertex -> (G.Vertex, LANode)
jnFix j@(JumpNode _ _) t = (rootLabel t, MacroNode (depth t) j)
jnFix _ _ = error "LANode is not JumpNode"

macros :: Tree G.Vertex -> Size -> Array G.Vertex Ladder -> [(G.Vertex, LANode)]
macros t n l = macros' [t] [] []
  where
    thr = microTreeSizeMax n
    macros' []                          [] xs = xs
    macros' (t@(Node v ts' _ s d _):ts) ps xs
      | s <= thr  = let j = JumpNode d (jumps t l)
                    in macros' ts []
                       $ (v, j):(concatMap (micros j) ts' ++ map (jnFix j) ps ++ xs)
      | otherwise = macros' (ts' ++ ts) (t:ps) xs
    macros' []                          ps _  = error "parents remain"

-- macros :: Tree G.Vertex -> Size -> Array G.Vertex Ladder -> [(G.Vertex, LANode)]
-- macros t@(Node v [] _ _ d _) _ l = [(v, JumpNode d $ jumps t l)]
-- macros t@(Node v ts _ s d _) n l
--   | s > thr   = let j    = case snd x of
--                              (MacroNode _ j')  -> j'
--                              j'@(JumpNode _ _) -> j'
--                              (MicroNode _ _ _) -> error "unexpected micro node"
--                     x:xs = concatMap macros' ts
--                 in (v, MacroNode d j):x:xs
--   | otherwise = let j = JumpNode d (jumps t l)
--                 in (v, j):concatMap (micros j) ts
--   where thr        = microTreeSizeMax n
--         macros' t' = macros t' n l
                   
micros :: LANode -> Tree G.Vertex -> [(G.Vertex, LANode)]
micros j (Node v ts (Just p) _ d _) = (v, MicroNode (rootLabel p) d j):concatMap (micros j) ts

laNodes :: Tree G.Vertex -> G.Bounds -> Array G.Vertex Ladder -> Array G.Vertex LANode
laNodes t b l = array b $ macros t (rangeSize b) l

findDepth :: Ladder -> Depth -> Maybe G.Vertex
findDepth l d
  | inRange (bounds l) d = Just (l!d)
  | otherwise            = error "inconsistent ladder"

nthParent :: LANode -> Array G.Vertex LANode -> Distance -> G.Vertex
nthParent m@(MicroNode p _ _) _ 1 = p
nthParent m@(MicroNode p _ _) n k = nthParent (n!p) n (k - 1)

levelAncestor :: Tree G.Vertex -> Size -> G.Bounds -> (G.Vertex -> Depth -> Maybe G.Vertex)
levelAncestor t s b = ans
  where
    l       = ld t b
    n       = laNodes t b l
    ans u k = case n!u of
      MacroNode d (JumpNode d' js)
        | d - k < 0   -> Nothing
        | otherwise   -> let i = log2Floor $ d' - (d - k)
                             v = js!i
                         in findDepth (l!v) (d - k)
      JumpNode d js
        | d - k < 0   -> Nothing
        | otherwise   -> let i = log2Floor k
                             v = js!i
                         in findDepth (l!v) (d - k)
      m@(MicroNode _ d (JumpNode d' js))
        | d - k < 0   -> Nothing
        | d - k >= d' -> Just $ nthParent m n k
        | otherwise   -> let i = log2Floor $ d' - (d - k)
                             v = js!i
                         in findDepth (l!v) (d - k)

--

-- https://atcoder.jp/contests/abc267/editorial/4714
solve :: Size -> [G.Edge] -> [(G.Vertex, Depth)] -> [G.Vertex]
solve n abs = map ans
  where 
    g          = buildUndirectedG (1, n) abs
    t          = head $ G.dfs g [1]
    l          = head . last $ T.levels t
    t'         = head $ G.dfs g [l]
    r          = head . last $ T.levels t'
    ansL       = levelAncestor (rebuild t') n (1, n)
    ansR       = levelAncestor (dfs g r) n (1, n)
    ans (u, k) = fromMaybe (-1) $ ansL u k <|> ansR u k

main :: IO ()
main = do
  n   <- readInt
  abs <- readTuples (n-1)
  q   <- readInt
  uks <- readTuples q
  putStr . unlines . map show $ solve n abs uks
