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
-- import qualified Data.Set as S
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

joinStr :: Show a => String -> [a] -> String
joinStr sep xs = intercalate sep $ map show xs

fork :: (a -> b) -> (a, a) -> (b, b)
fork f (x, y) = (f x, f y)

type Transposition = (Int, Int)

swap :: Transposition -> [a] -> [a]
swap (i, j) a
  | i == j    = a
  | otherwise = take k a ++ [a!!l] ++ drop (k+1) (take l a) ++ [a!!k] ++ drop (l+1) a
  where k = min i j
        l = max i j

pad :: Int -> String
pad t
  | 0 <= t && t <= 9 = '0':show t
  | otherwise        = show t

valid :: String -> Bool
valid s = 0 <= h && h <= 23 && 0 <= m && m <= 59
  where (h, m) = fork read $ splitAt 2 s

solve :: Int -> Int -> [Int]
solve h m = head $ [[h, y] | y <- [m..59], p h y]
            ++ [[x, y] | x <- [h+1..23] ++ [0..h], y <- [0..59], p x y]
  where p x y = valid . swap (1, 2) $ pad x ++ pad y

main :: IO ()
main = do
  [h, m] <- readIntList
  putStrLn . joinStr " " $ solve h m
