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
import Data.Array
-- import Data.Array.ST
import Data.Bits
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
import qualified Data.Vector.Unboxed as UV
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

modulo :: Int
modulo = 998244353

md :: Int -> Int
md x = x `mod` modulo

madd :: Int -> Int -> Int
madd x y = (x+y) `mod` modulo

encode :: Char -> Int
encode c = ord c - ord 'A'

type BitSet = Int
type Size   = Int
type Member = Int

bsUniv :: Size -> BitSet
bsUniv n = 2^n - 1

bsSize :: BitSet -> Size
bsSize b = finiteBitSize b - 1

bsCount :: BitSet -> Int
bsCount = length . bsMembers

bsEmpty :: BitSet -> Bool
bsEmpty = (== 0)

bsSingleton :: Member -> BitSet
bsSingleton = bsInsert 0

bitset :: [Member] -> BitSet
bitset = foldr bsInsert 0

bsMember :: Member -> BitSet -> Bool
bsMember = flip testBit

bsMembers :: BitSet -> [Member]
bsMembers b = filter (flip bsMember b) [0..bsSize b]

bsDiff :: BitSet -> BitSet -> BitSet
bsDiff b b' = foldl' clearBit b (bsMembers b')

bsInsert :: Member -> BitSet -> BitSet
bsInsert b x = setBit b x

bsDelete :: Member -> BitSet -> BitSet
bsDelete b x = bsDiff b $ bsSingleton x

-- https://atcoder.jp/contests/abc215/editorial/2483
solve :: Int -> [Int] -> Int
solve n xs = foldl' madd 0 [dp!(b, r) | b <- bs, r <- rs]
  where
    dp        = foldl step dp0 xs
    l         = (0, 0)
    u         = (1023, 9)
    bs        = [0..1023]
    rs        = [0..9]
    dp0       = listArray (l, u) (repeat 0)
    step dp x = accumArray madd 0 (l, u) $ adds dp x
    adds dp x = [((b, r), dp!(b, r)) | b <- bs, r <- rs]
                ++ [((b, x), dp!(b, x)) | b <- bs]
                ++ [((bsInsert b x, x), dp!(b, r)) | b <- bs, not $ bsMember x b, r <- rs]
                ++ [((bsSingleton x, x), 1)]
    
main :: IO ()
main = do
  n <- readInt
  s <- getLine
  print $ solve n (map encode s)
  
