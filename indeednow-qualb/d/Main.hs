{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeFamilies #-}

-- import Control.Applicative ((<*>), Alternative, empty, (<|>))
import Control.Monad
-- import Control.Monad.ST
import Data.Array
-- import Data.Array.ST
-- import Data.Bits
import Data.Char
-- import Data.Foldable
-- import Data.Function
-- import Data.Int
-- import Data.Ix
import Data.List
import Data.Maybe
-- import Data.Tuple
-- import Data.Void
-- import Data.Word
import Debug.Trace
-- import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

putStrLns :: Show a => [a] -> IO ()
putStrLns = putStr . unlines . map show

tri :: Int -> Int
tri n = n*(n+1)`div`2

positions :: Ix a => [a] -> Array a [Int]
positions (x:xs) = accumArray (flip (:)) [] (l, u) $ zip ys [n, n-1..1]
  where
    (l, u, n, ys)        = foldl' step (x, x, 1, [x]) xs
    step (l, u, n, ys) y = (min l y, max u y, n+1, y:ys)

sumOn :: Num b => (a -> b) -> [a] -> b
sumOn f = foldl' step 0
  where step s x = s + f x

pats :: Int -> [Int] -> Int
pats n is = tri n - (sumOn tri $ zipWith sub (is ++ [n+1]) ([0] ++ is))
  where sub i j = i - j - 1
    
solve :: Int -> Int -> [Int] -> [Int]
solve n c as = map ((pats n) . (ps!)) [1..c]
  where ps = positions as

main :: IO ()
main = do
  [n, c] <- readIntList
  as     <- readIntList
  putStrLns $ solve n c as
