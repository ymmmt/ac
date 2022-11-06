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

yn :: Bool -> String
yn True  = "Yes"
yn False = "No"

sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . group . sort

pairs1 :: [a] -> [(a, a)]
pairs1 []     = []
pairs1 [x]    = []
pairs1 (x:xs) = [(x, y) | y <- xs]

pairs :: [a] -> [(a, a)]
pairs = concatMap pairs1 . tails . tail 

solve :: Int -> [[Int]] -> Bool
solve n kxss = 2 * length ps == n * (n-1)
  where ps = sortUniq (concatMap pairs kxss)

main :: IO ()
main = do
  [n, m] <- readIntList
  kxss <- readIntLists m
  putStrLn . yn $ solve n kxss
