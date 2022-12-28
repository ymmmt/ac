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
import qualified Data.Graph as G
import qualified Data.Map as M
-- import qualified Data.Ratio as R
import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

tup :: [a] -> (a, a)
tup [x, y] = (x, y)

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

readIntListsAll :: IO [[Int]]
readIntListsAll = map toIntList . BS.lines <$> BS.getContents

yn :: Bool -> String
yn True  = "Yes"
yn False = "No"

-- Return False if cycle whose length >=2 is detected.
consistent :: M.Map G.Edge Int -> G.Graph -> G.Vertex -> Bool
consistent dists g r = dfs (S.singleton r) (M.singleton r 0) [(r,  0)]
  where
    dfs _    _  []           = True
    dfs seen ds ((u, d):vds) = all ok vs && dfs seen' ds' (vds' ++ vds)
      where
        dist v           = d + dists M.! (u, v)
        ok v             = dist v == ds M.! v
        (vs, ws)         = partition (flip S.member seen) (g!u)
        seen'            = foldr S.insert seen ws
        (ds', vds')      = foldr step (ds, []) ws
        step w (ds, vds) = (M.insert w (dist w) ds, (w, dist w):vds)
          
solve :: Int -> [[Int]] -> Bool
solve n lrds = all (consistent dists g) rs
  where
    dists               = M.fromList [((l, r), d) | [l, r, d] <- lrds]
    g                   = G.buildG (1, n) $ map (tup . (take 2)) lrds
    (rs, _)             = foldl' step ([], S.empty) $ G.topSort g
    step a@(rs, seen) v = if S.member v seen then a
                          else (v:rs, seen')
      where
        vs    = T.flatten . head $ G.dfs g [v]
        seen' = foldr S.insert seen vs

main :: IO ()
main = do
  [n, m] <- readIntList
  lrds   <- readIntListsAll
  putStrLn . yn $ solve n lrds
