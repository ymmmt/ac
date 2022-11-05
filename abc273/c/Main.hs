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

counter :: Ord a => [a] -> Map.Map a Int
counter = foldr (\x mp -> Map.insertWith f x 1 mp) Map.empty
  where f _ old = old + 1

sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . group . sort

solve :: Int -> [Int] -> [Int]
solve n as = reverse $ take (n-n') (repeat 0) ++ cs
  where as' = sortUniq as
        n' = length as'
        c = counter as
        cs = map (\a -> Map.findWithDefault 0 a c) as'

main :: IO ()
main = do
  n <- readInt
  as <- readIntList
  putStr . unlines . map show $ solve n as
