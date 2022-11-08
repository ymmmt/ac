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

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

joinStr :: Show a => String -> [a] -> String
joinStr sep xs = intercalate sep $ map show xs

monotone :: Ord a => [a] -> Bool
monotone []            = True
monotone [x]           = True
monotone (x:xs@(y:ys)) = x < y && monotone xs

longestMonotoneSuffix :: Ord a => [a] -> [a]
longestMonotoneSuffix xs = head $ filter monotone $ tails xs

solve :: Int -> [Int] -> [Int]
solve n ps = ps' ++ [p'] ++ reverse (sort ([p] ++ (delete p' lms)))
  where lms = longestMonotoneSuffix ps
        k   = length lms
        p   = ps!!(n - k - 1)
        p'  = maximum $ filter (<p) lms
        ps' = take (n - k - 1) ps

main :: IO ()
main = do
  n <- readInt
  ps <- readIntList
  putStrLn . (joinStr " ") $ solve n ps
