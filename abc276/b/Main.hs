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

ans :: Array Int [Int] -> Int -> [Int]
ans graph i = length vs : sort vs
  where vs = graph!i

solve :: Int -> [[Int]] -> [[Int]]
solve n es = map (ans g) [1..n]
  where g = accumArray (flip (:)) [] (1, n) es'
        es' = concatMap (\[u, v] -> [(u, v), (v, u)]) es

main :: IO ()
main = do
  [n, m] <- readIntList
  es     <- readIntLists m
  putStr . unlines . map (joinStr " ") $ solve n es
  
