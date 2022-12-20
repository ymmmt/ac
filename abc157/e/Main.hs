{-# OPTIONS_GHC -O2 #-}

-- {-# LANGUAGE BangPatterns #-}
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
import qualified Data.Set as S
-- import qualified Data.Sequence as Seq
-- import qualified Data.Tree as T
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified Numeric as N

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

putStrLns :: Show a => [a] -> IO ()
putStrLns = putStr . unlines . map show

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

unwrap :: [a] -> a
unwrap [x] = x

intervalMember :: (Ord a, Bounded a) => a -> a -> S.Set a -> Bool
intervalMember l r s = x <= y
  where x = fromMaybe maxBound $ S.lookupGE l s
        y = fromMaybe minBound $ S.lookupLE r s

type Index = Int
data Query = Q1 Index Char
           | Q2 Index Index
type SetArray = Array Char (S.Set Index)
type State = (SetArray, [Int])

parse :: String -> Query
parse (x:xs) = case x of
  '1' -> Q1 (read y) (unwrap z)
  '2' -> Q2 (read y) (read z)
  where
    [y, z] = words xs

insertCharAt :: Char -> Index -> SetArray -> SetArray
insertCharAt c i a = a // [(c, S.insert i (a!c))]

deleteCharAt :: Char -> Index -> SetArray -> SetArray
deleteCharAt c i a = a // [(c, S.delete i (a!c))]

charAt :: Index -> SetArray -> Maybe Char
charAt i a = fst <$> find search (assocs a)
  where search (_, s) = S.member i s
          
deleteAt :: Index -> SetArray -> SetArray
deleteAt i a = deleteCharAt (fromJust $ charAt i a) i a

charsIn :: Index -> Index -> SetArray -> Int
charsIn l r = count (intervalMember l r) . elems

step :: State -> Query -> State
step (a, is) (Q1 i c) = (insertCharAt c i (deleteAt i a), is)
step (a, is) (Q2 l r) = (a, charsIn l r a:is)

extract :: State -> [Int]
extract = reverse . snd

-- https://img.atcoder.jp/abc157/editorial.pdf
solve :: Int -> String -> [Query] -> [Int]
solve n s = extract . foldl step (a, [])
  where
    a = listArray ('a', 'z') . map S.fromList . elems
        . accumArray (flip (:)) [] ('a', 'z') $ zip s [1..]

main :: IO ()
main = do
  n  <- readInt
  s  <- getLine
  q  <- readInt
  qs <- replicateM q (parse <$> getLine)
  putStrLns $ solve n s qs
