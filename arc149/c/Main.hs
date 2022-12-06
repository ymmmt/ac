{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
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
-- import Data.Function
-- import Data.Int
-- import Data.Ix
import Data.List
import Data.Maybe
-- import Data.Tuple
-- import Data.Void
-- import Data.Word
-- import Debug.Trace
-- import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Graph as G
-- import qualified Data.Map as Map
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

joinStr :: Show a => String -> [a] -> String
joinStr sep xs = intercalate sep $ map show xs

type Cell     = (Int, Int)
type Matrix a = Array Cell a

printMatrix :: Show a => String -> Matrix a -> IO ()
printMatrix s m = mapM_ printRow rs
  where
    ((x, y), (z, w)) = bounds m
    rs               = range (x, z)
    cs               = range (y, w)
    printRow r       = putStrLn . joinStr s $ map (m!) [(r, j) | j <- cs]

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

diff :: Ord a => [a] -> [a] -> [a]
diff xs [] = xs
diff [] _  = []
diff xs@(x:xs') ys@(y:ys')
  | x < y     = x:diff xs' ys
  | x > y     = diff xs ys'
  | otherwise = diff xs' ys'

solve :: Int -> Matrix Int
solve n = listArray ((1, 1), (n, n)) $ xs ++ ys ++ zs ++ ws
  where
    xs = diff [1, 3..n^2] $ sort ys
    ys = case n of
           3 -> [1, 3, 7]
           4 -> [1, 3, 5, 7]
           5 -> [21, 1, 3, 9, 15]
           _ -> take n [3, 9..]
    zs = case n of
           3 -> [8, 6, 2]
           4 -> [8, 6, 4, 2]
           5 -> [24, 8, 6, 12, 18]
           _ -> take n [6, 12..]
    ws = diff [2, 4..n^2] $ sort zs

main :: IO ()
main = do
  n <- readInt
  printMatrix " " $ solve n
