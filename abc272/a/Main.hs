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

main :: IO ()
main = do
  readInt
  as <- readIntList
  print (sum as)
