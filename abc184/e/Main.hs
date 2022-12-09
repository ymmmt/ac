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
-- import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
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
{-# INLINE pair #-}

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)
{-# INLINE cross #-}

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z []     = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

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
{-# INLINE encode #-}

buildG :: Int -> Int -> CMatrix -> G.Graph
buildG h w c = buildUndirectedG (1, h*w) [(eij, encode w kl) | (ij, l) <- assocs c
                                                             , l /= '#'
                                                             , let eij = encode w ij
                                                             , kl <- adjs ij]
  where ((1, 1), (h, w)) = bounds c
        inBounds (i, j)  = 1 <= i && i <= h && 1 <= j && j <= w
        adjs (i, j)      = filter ((&&) <$> inBounds <*> (/= '#') . (c!))
                           [(i+1, j), (i, j+1)]

buildG' :: Int -> Int -> CMatrix -> G.Graph
buildG' h w c = buildUndirectedG (1, h*w) [(eij, encode w kl) | (ij, l) <- assocs c
                                                             , l /= '#'
                                                             , let eij = encode w ij
                                                             , kl <- adjs ij]
  where ((1, 1), (h, w)) = bounds c
        adjs (i, j)      = (if i < h && c!(i+1, j) /= '#' then [(i+1, j)] else []) ++
                           (if j < w && c!(i, j+1) /= '#' then [(i, j+1)] else [])

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
{-# INLINE swap #-}

warp :: Char -> Bool
warp = inRange ('a', 'z')
{-# INLINE warp #-}

-- see mv i = UMV.unsafeWrite mv i True

-- notSeen mv i = not <$> UMV.unsafeRead mv i

-- solve :: Int -> Int -> CMatrix -> Int
-- solve h w a = bfs
--   where
--     g     = buildG w a
--     warps = accumArray (flip (:)) [] ('a', 'z')
--             . map (cross (id, encode w) . swap)
--             . filter (warp . snd) $ assocs a
--     l     = UA.listArray (1, h*w) $ elems a :: UA.UArray Int Char
--     s     = encode w . fst . head . filter ((== 'S') . snd) $ assocs a
--     t     = encode w . fst . head . filter ((== 'G') . snd) $ assocs a
--     bfs   = runST $ do
--       seen <- UMV.replicate (h*w+1) False
--       see seen s
--       let go d w vs
--             | t `elem` vs = return d
--             | null vs     = return (-1)
--             | otherwise   = do
--                 let step (w, vs) v = do
--                       let b  = warp (l UA.! v) && S.notMember (l UA.! v) w
--                           w' = if b then S.insert (l UA.! v) w else w
--                       vs' <- filterM (notSeen seen) $ if b then g!v ++ warps!(l UA.! v) else g!v
--                       mapM_ (see seen) vs'
--                       return (w', vs' ++ vs)
--                 (w', vs') <- foldM' step (w, []) vs
--                 go (d+1) w' vs'
--       go 0 S.empty [s]

-- solve :: Int -> Int -> CMatrix -> Int
-- solve h w a = bfs
--   where
--     g     = buildG w a
--     warps = accumArray (flip (:)) [] ('a', 'z')
--             . map (cross (id, encode w) . swap)
--             . filter (warp . snd) $ assocs a
--     l     = UA.listArray (1, h*w) $ elems a :: UA.UArray Int Char
--     s     = encode w . fst . head . filter ((== 'S') . snd) $ assocs a
--     t     = encode w . fst . head . filter ((== 'G') . snd) $ assocs a
--     seen  = UV.replicate (h*w+1) False UV.// [(s, True)]
--     bfs   = go 0 S.empty seen [s]
--     go d w seen vs
--       | t `elem` vs = d
--       | null vs     = -1
--       | otherwise   = go (d+1) w' seen' vs''
--       where
--         (w', vs') = foldr step (w, []) vs
--         step v (w, us) = (w', vs' ++ us)
--           where
--             b     = warp (l UA.! v) && S.notMember (l UA.! v) w
--             w'    = if b then S.insert (l UA.! v) w else w
--             vs'   = filter (not . (seen UV.!)) $ if b then g!v ++ warps!(l UA.! v) else g!v
--         vs''      = sortUniq vs'
--         seen'     = seen UV.// zip vs'' (repeat True)
--         -- w'     = foldr S.insert w . filter warp $ map (l UA.!) vs'
--         -- vs'    = sortUniq $ concatMap adjs vs
--         -- seen'  = seen UV.// zip vs' (repeat True)
--         -- adjs v = filter (not . (seen UV.!))
--         --          $ if warp (l UA.! v) && S.notMember (l UA.! v) w
--         --            then g!v ++ warps!(l UA.! v)
--         --            else g!v

see :: (MArray a Bool m, Ix i) => a i Bool -> i -> m ()
see marr i = writeArray marr i True
{-# INLINE see #-}

notSeen :: (MArray a Bool m, Ix i) => a i Bool -> i -> m Bool
notSeen marr i = not <$> readArray marr i
{-# INLINE notSeen #-}

solve :: Int -> Int -> CMatrix -> Int
solve h w a = bfs
  where
    g     = buildG' h w a
    warps = accumArray (flip (:)) [] ('a', 'z')
            . map (cross (id, encode w) . swap)
            . filter (warp . snd) $ assocs a
    l     = UV.fromList $ 'x':elems a
    s     = encode w . fst . fromJust . find ((== 'S') . snd) $ assocs a
    t     = encode w . fst . fromJust . find ((== 'G') . snd) $ assocs a
    bfs   = runST $ do
      seen <- newArray (1, h*w) False :: ST s (STUArray s Int Bool)
      w    <- newArray ('#', 'z') False :: ST s (STUArray s Char Bool)
      see seen s
      let go d vs
            | t `elem` vs = return d
            | null vs     = return (-1)
            | otherwise   = do
                let step vs v = do
                      b  <- (warp (l `UV.unsafeIndex` v) &&) <$> notSeen w (l `UV.unsafeIndex` v)
                      vs' <- filterM (notSeen seen) $ if b then g!v ++ warps!(l `UV.unsafeIndex` v) else g!v
                      when b $ see w (l `UV.unsafeIndex` v)
                      mapM_ (see seen) vs'
                      return $ vs' ++ vs
                vs' <- foldM' step [] vs
                go (d+1) vs'
      go 0 [s]

-- solve :: Int -> Int -> CMatrix -> Int
-- solve h w a = bfs
--   where
--     g     = buildG w a
--     warps = accumArray (flip (:)) [] ('a', 'z')
--             . map (cross (id, encode w) . swap)
--             . filter (warp . snd) $ assocs a
--     l     = UA.listArray (1, h*w) $ elems a :: UA.UArray Int Char
--     s     = encode w . fst . head . filter ((== 'S') . snd) $ assocs a
--     t     = encode w . fst . head . filter ((== 'G') . snd) $ assocs a
--     seen  = S.singleton s
--     bfs   = go 0 S.empty seen [s]
--     go d w seen vs
--       | t `elem` vs = d
--       | null vs     = -1
--       | otherwise   = go (d+1) w' seen' vs'
--       where
--         (w', seen', vs') = foldr step (w, seen, []) vs
--         step v (w, seen, us) = (w', seen', vs' ++ us)
--           where
--             b     = warp (l UA.! v) && S.notMember (l UA.! v) w
--             w'    = if b then S.insert (l UA.! v) w else w
--             vs'   = filter (flip S.notMember seen) $ if b then g!v ++ warps!(l UA.! v) else g!v
--             seen' = foldr S.insert seen $ vs'

main :: IO ()
main = do
  [h, w] <- readIntList
  a      <- readCMatrix h w
  print $ solve h w a
