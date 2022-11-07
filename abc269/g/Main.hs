{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Tuple
--import Data.Vector.Unboxed (Vector, (!), (//), fromList)
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

ipartition :: Int -> [Int]
ipartition 0 = []
ipartition n = go n 1
  where go 0 _ = []
        go n k = x:go (n-x) (2*k)
          where x = min n k

pairs :: Map.Map Int Int -> Int -> [(Int, Int)]
pairs mp c = map (c,) $ ipartition (Map.findWithDefault 0 c mp)

convert :: [Int] -> [(Int, Int)]
convert xs = concatMap (pairs c) ks
  where c = counter xs
        ks = map fst $ Map.toList c

-- update :: Int -> (Int, Int) -> STUArray ? Int Int -> STUArray ? Int Int
-- update m (c, count) dp = dp // map new [l..r]
--   where d = c * count
--         l = max 0 (- d)
--         r = min m (m - d)
--         new s = (s+d, min (dp!(s+d)) (dp!s + count))

update :: Int -> (STUArray s Int Int) -> (Int, Int) -> ST s (STUArray s Int Int)
update m dp (c, count) =
  let d   = c * count
      l   = max 0 (- d)
      r   = min m (m - d)
      ss  = if d>=0 then [r, r-1..l] else [l..r]
  in do forM_ ss $ \s -> do x <- readArray dp (s+d)
                            y <- readArray dp s
                            writeArray dp s $ min x (y+count)
        return dp

solve :: Int -> Int -> [[Int]] -> [Int]
solve n m abs = map (ans n dp) [0..m]
  where cs = map (\ab -> ab!!1 - ab!!0) abs
        fs = convert cs
        sa = sum $ map head abs
        inf = n+1
        --dp0 = (fromList $ take (m+1) (repeat inf)) // [(sa, 0)]
        dp = runSTUArray $ do
          dp' <- newArray (0, m) inf :: ST s (STUArray s Int Int)
          writeArray dp' sa 0
          forM_ fs (update m dp')
          return dp'
        ans n dp s = if dp!s <= n then dp!s else -1

main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine
  abs <- readIntLists n
  putStr . unlines . map show $ solve n m abs
