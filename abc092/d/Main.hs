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
-- import Debug.Trace
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

joinStr :: Show a => String -> [a] -> String
joinStr sep = intercalate sep . map show

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

printCMatrix :: CMatrix -> IO ()
printCMatrix m = mapM_ printRow rs
  where
    ((x, y), (z, w)) = bounds m
    rs               = range (x, z)
    cs               = range (y, w)
    printRow r       = mapM_ (putChar . (m!)) [(r, j) | j <- cs] >> putStrLn ""

black :: Char
black = '#'

white :: Char
white = '.'

solve :: Int -> Int -> CMatrix
solve a b = array ((1, 1), (100, 100)) $
            [((i, j), black) | i <- [1..50], j <- [1..100]] ++
            [((i, j), white) | i <- [51..100], j <- [1..100]] ++
            take (a-1) [((i, j), white) | i <- [1..50], j <- [1..100], (i+j) `mod` 3 == 1, j /= 1] ++
            take (b-1) [((i, j), black) | i <- [100, 99..51], j <- [100, 99..1], (i+j) `mod` 3 == 1, j /= 100]

main :: IO ()
main = do
  [a, b] <- readIntList
  putStrLn $ joinStr " " [100, 100]
  printCMatrix $ solve a b
