{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeFamilies #-}

-- import Control.Applicative ((<*>), Alternative, empty, (<|>))
import Control.Monad
-- import Control.Monad.ST
-- import Control.Monad.State hiding (get)
import Data.Array
-- import Data.Array.ST
import Data.Bits
import Data.Char
-- import Data.Foldable
-- import Data.Function
-- import Data.Int
-- import Data.Ix
import Data.List
import Data.Maybe
-- import Data.Tuple
-- import Data.Tuple.Extra
-- import Data.Void
import Data.Word
import Debug.Trace
-- import System.Random -- random-1.0.1.1
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Graph as G
import qualified Data.Map as M
-- import qualified Data.Ratio as R
-- import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f l r = if l < r && f l then go l r
                else error "bsearch: invalid arguments"
  where go l r
          | l+1 == r  = l
          | f m       = go m r
          | otherwise = go l m
          where m = (l+r) `div` 2

type Hash  = Int

base :: Int
base = 1000000007

-- https://en.wikipedia.org/wiki/Rolling_hash
rhash :: String -> (Int -> Int -> Hash)
rhash s = get
  where
    n        = length s
    rh       = UA.listArray (1, n) . tail $ scanl step 0 s :: UA.Array Int Hash
    step h c = (base*h + ord c)
    -- bs       = listArray (1, n) $ iterate (*base) base
    bs       = UA.array (1, n) [(i, p i) | i <- [1..n]] :: UA.Array Int Int
      where p i = if i == 1 then base else base * bs UA.! (i-1)
    get i j
      | i <= j && inRange (1, n) i && inRange (1, n) j = if i == 1 then rh UA.! j
                                                         else (rh UA.! j - bs UA.! (j-i+1) * rh UA.! (i-1))

-- https://drken1215.hatenablog.com/entry/2019/09/16/014600
solve :: Int -> String -> Int
solve n s = bsearch check 0 (half+1)
  where
    get       = rhash s
    half      = n `div` 2
    check 0   = True
    check len = search M.empty [1..n-len+1]
      where
        search mp []     = False
        search mp (j:js) = maybe False (\i -> j - i >= len) (M.lookup h mp) || search mp' js
          where
            h   = get j (j+len-1)
            mp' = if M.notMember h mp then M.insert h j mp else mp

main :: IO ()
main = do
  n <- readInt
  s <- getLine
  print $ solve n s
