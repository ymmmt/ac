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

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x, y) (z, w) = abs (x-z) + abs (y-w)

findTokens :: [String] -> [(Int, Int)]
findTokens ss = concatMap find (zip ss [0..])
  where find (s, i) = map (i,) $ elemIndices 'o' s

solve :: [String] -> Int
solve = uncurry manhattan . pair ((!!0), (!!1)) . findTokens

main :: IO ()
main = do
  [h, w] <- readIntList
  ss     <- replicateM h getLine
  print $ solve ss
