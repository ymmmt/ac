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
-- import Data.Array
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

pfactors :: Int -> [Int]
pfactors n = case fs of
               []  -> [n]
               f:_ -> f:pfactors (n `div` f)
  where fs = take 1 $ filter ((== 0) . (n `mod`)) [2..n']
        n' = floor . sqrt $ fromIntegral n

-- https://atcoder.jp/contests/abc280/editorial/5333
solve :: Int -> Int
solve k = if p >= 10^6 then p else go 1 k
  where p = maximum $ pfactors k
        go n k = if k' == 1 then n else go (n+1) k'
          where k' = k `div` gcd n k

main :: IO ()
main = readInt >>= print . solve
