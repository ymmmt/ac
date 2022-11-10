{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Word
import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
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

yn :: Bool -> String
yn True  = "Yes"
yn False = "No"

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

readCMatrix :: Int -> Int -> IO CMatrix
readCMatrix h w = do
  rs <- replicateM h getLine
  let as               = concatMap colAssocs $ zip [1..h] rs
      colAssocs (i, r) = map (cross ((i,), id)) $ zip [1..w] r
  return $ array ((1, 1), (h, w)) as

adjacents :: CMatrix -> Cell -> [Cell]
adjacents c (i, j)
  | c!(i, j) == '#' = []
  | otherwise       = filter ((&&) <$> inRange (bounds c) <*> (=='.') . (c!))
                      $ [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)

choices :: Int -> [a] -> [[a]]
choices 0 _   = [[]]
choices _ []  = []
choices n (x:xs)
  | n > 0     = (map (x:) $ choices (n-1) xs) ++ choices n xs
  | otherwise = []

encode :: Int -> Cell -> G.Vertex
encode w (i, j) = w * (i - 1) + j

edges :: CMatrix -> [(Cell, Cell)]
edges c = concatMap (\ij -> map (ij,) $ adjacents c ij) $ indices c

encodeEdges :: Int -> [(Cell, Cell)] -> [G.Edge]
encodeEdges w = map (cross (encode w, encode w))

start :: CMatrix -> Cell
start = maybe (0, 0) fst . find ((== 'S') . snd) . assocs

solve :: Int -> Int -> CMatrix -> Bool
solve h w c = any (\[u, v] -> G.path g u v) uvs
  where s   = start c
        es  = encodeEdges w $ edges c
        g   = G.buildG (1, h*w) es
        uvs = choices 2 $ map (encode w) $ adjacents c s
        
main :: IO ()
main = do
  [h, w] <- readIntList
  c      <- readCMatrix h w
  putStrLn . yn $ solve h w c
