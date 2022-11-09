{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad
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

zeros :: String -> Int
zeros = length . takeWhile (=='0') . reverse

count2 :: Int -> Int
count2 0 = 0
-- count2 x = zeros $ N.showBin x ""
count2 x = zeros $ N.showIntAtBase 2 intToDigit x ""

count3 :: Int -> Int
count3 0 = 0
count3 x = zeros $ N.showIntAtBase 3 intToDigit x ""

multiple :: Int -> Bool
multiple x = 2 ^ count2 x * 3 ^ count3 x == x

count :: Int -> Int -> Maybe Int
count g a
  | r == 0 && multiple q = Just $ ((+) <$> count2 <*> count3) q
  | otherwise            = Nothing
    where (q, r) = divMod a g

solve :: [Int] -> Int
solve as = sum . fromMaybe [-1] $ mapM (count g) as
  where g = foldl1' gcd as

main :: IO ()
main = do
  readInt
  as <- readIntList
  print $ solve as
