{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Vector.Unboxed (Vector, (!), (//), fromList)
import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

roundUpOn5 :: (RealFrac a, Integral b) => a -> b
roundUpOn5 x
  | n <= -0.5 = m - 1
  | n >= 0.5  = m + 1
  | otherwise = m
  where (m, n) = properFraction x

roundAt :: Int -> Int -> Int
roundAt x k = if k < 0
              then x
              else 10^(k+1) * roundUpOn5 (fromIntegral x / 10^(k+1))

solve :: Int -> Int -> Int
solve x k = go 0 x
  where go i x = if i==k then x
                 else go (i+1) (roundAt x i)

main :: IO ()
main = do
  [x, k] <- readIntList
  print $ solve x k
