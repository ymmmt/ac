-- IO

yn :: Bool -> String
yn True  = "Yes"
yn False = "No"

joinStr :: Show a => String -> [a] -> String
joinStr sep xs = intercalate sep $ map show xs

-- Random

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt xy = state (randomR xy)

type Probability = Double

judgeSt :: Probability -> State StdGen Bool
judgeSt p = (< p) <$> randomRSt (0.0, 1.0)

-- Char Matrix

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

readCMatrix :: Int -> Int -> IO CMatrix
readCMatrix h w = do
  rs <- replicateM h getLine
  let as               = concatMap colAssocs $ zip [1..h] rs
      colAssocs (i, r) = map (cross ((i,), id)) $ zip [1..w] r
  return $ array ((1, 1), (h, w)) as

printCMatrix :: CMatrix -> IO ()
printCMatrix m = mapM_ printRow rs
  where rs         = range . pair (fst . fst, fst . snd) $ bounds m
        cs         = range . pair (snd . fst, snd . snd) $ bounds m
        printRow r = mapM_ (putChar . (m!)) [(r, j) | j <- cs] >> putStrLn ""

adjacents :: CMatrix -> Cell -> [Cell]
adjacents c (i, j)
  | c!(i, j) == '#' = []
  | otherwise       = filter ((&&) <$> inRange (bounds c) <*> (=='.') . (c!))
                      $ [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

encode :: Int -> Cell -> G.Vertex
encode w (i, j) = w * (i - 1) + j

edges :: CMatrix -> [(Cell, Cell)]
edges c = concatMap (\ij -> map (ij,) $ adjacents c ij) $ indices c

encodeEdges :: Int -> [(Cell, Cell)] -> [G.Edge]
encodeEdges w = map (cross (encode w, encode w))

-- Matrix

rowCells :: (Int, [a]) -> [(a, Cell)]
rowCells (i, xs) = map (\(j, x) -> (x, (i, j))) $ zip [1..] xs

-- Tuple

tupWith :: (a -> b) -> a -> (a, b)
tupWith f x = (x, f x)

-- MArray (for Graph)

see :: (MArray a Bool m, Ix i) => a i Bool -> i -> m ()
see marr i = writeArray marr i True

notSeen :: (MArray a Bool m, Ix i) => a i Bool -> i -> m Bool
notSeen marr i = not <$> readArray marr i

-- Array

mapArray :: (Ix a) => (a, a) -> (a -> b) -> Array a b
mapArray b f = listArray b . map f $ range b

-- List

sg :: Ord a => [a] -> [[a]]
sg = group . sort

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . sg

repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1)

repeatedBy :: Ord a => (Int -> Bool) -> [a] -> [a]
repeatedBy p = map head . filterByLength p

allUnique :: Ord a => [a] -> Bool
allUnique = all (null . tail) . sg

sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . sg

diff :: Ord a => [a] -> [a] -> [a]
diff xs [] = xs
diff [] _  = []
diff xs@(x:xs') ys@(y:ys')
  | x < y     = x:diff xs' ys
  | x > y     = diff xs ys'
  | otherwise = diff xs' ys'

coordComp :: Ord a => Int -> [a] -> Map.Map Int a
coordComp i0 = Map.fromAscList . zip [i0..] . sortUniq

pairs1 :: [a] -> [(a, a)]
pairs1 []     = []
pairs1 [x]    = []
pairs1 (x:xs) = [(x, y) | y <- xs]

pairs :: [a] -> [(a, a)]
pairs = concatMap pairs1 . tails

choices :: Int -> [a] -> [[a]]
choices 0 _   = [[]]
choices _ []  = []
choices n (x:xs)
  | n > 0     = (map (x:) $ choices (n-1) xs) ++ choices n xs
  | otherwise = []

bfilter :: [a] -> [Bool] -> [a]
bfilter xs bs = map fst . filter snd $ zip xs bs

minimumOn :: Ord a => (b -> a) -> [b] -> (b, a, Int)
minimumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx <= ky then u else v

maximumOn :: Ord a => (b -> a) -> [b] -> (b, a, Int)
maximumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx >= ky then u else v

sumOn :: Num a => (b -> a) -> [b] -> a
sumOn f xs = foldl' step 0 xs
  where step acc x = acc + f x

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

type MultiSet a = [(a, Int)]

multiSet :: Ord a => [a] -> MultiSet a
multiSet = map (pair (head, length)) . group . sort

toList :: MultiSet a -> [a]
toList = concatMap (\(x, n) -> replicate n x)

groupSeq :: (a -> a -> Bool) -> [a] -> [[a]]
groupSeq _ []     = []
groupSeq _ [x]    = [[x]]
groupSeq f (x:xs) = if f x y then (x:y:ys):yss else [x]:gs
  where gs@((y:ys):yss) = groupSeq f xs

zapp :: [a -> b] -> [a] -> [b]
zapp = zipWith ($)

-- Map

counter :: Ord a => [a] -> Map.Map a Int
counter = foldr (\x mp -> Map.insertWith f x 1 mp) Map.empty
  where f _ old = old + 1

-- Function

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)

fork :: (a -> b) -> (a, a) -> (b, b)
fork f (x, y) = (f x, f y)

-- indicator function
ind :: (a -> Bool) -> (a -> Int)
ind p x = if p x then 1 else 0

-- Graph

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

complement :: G.Graph -> G.Graph
complement g = G.buildG (bounds g) [(u, v) | u <- xs, v <- xs, u /= v, not (g!(u, v))]
  where xs = range (bounds g)

bothEnds :: G.Graph -> G.Vertex -> (G.Vertex, G.Vertex)
bothEnds g s = (l, r)
  where t  = head $ G.dfs g [s]
        l  = head . last $ T.levels t
        t' = head $ G.dfs g [l]
        r  = head . last $ T.levels t'

findDeepest :: G.Graph -> G.Vertex -> G.Vertex
findDeepest g r = snd . head . last . takeWhile (not . null)
                  $ iterate (concatMap children) [(-1, r)]
  where children (p, v) = map (v,) $ filter (/= p) (g!v)

-- Math

-- Use fromEnum instead.
-- qt :: Bool -> Int
-- qt True  = 1
-- qt False = 0

d2 :: [Double] -> [Double] -> Double
d2 xs ys = sum $ zipWith d2' xs ys
  where d2' x y = (x - y) ** 2

dot :: [Int] -> [Int] -> Int
dot xs ys = sum $ zipWith (*) xs ys

arithSeqSum :: Integral a => a -> a -> a -> a
arithSeqSum n d a0 = n * a0 + d * n * (n-1) `div` 2

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

fact :: Int -> Integer
fact n
  | n <= 1    = 1
  | otherwise = toInteger n * fact (n - 1)

choose :: Int -> Int -> Integer
choose n k
  | n < k || n < 0 || k < 0 = 0
  | otherwise               = fact n `div` fact k `div` fact (n - k)

cut :: Ord a => a -> a -> a -> a
cut l h | l > h     = undefined
        | otherwise = max l . min h

hyperFloor :: Int -> Int
hyperFloor x = 2 ^ (floor . logBase 2 $ fromIntegral x)

hyperCeiling :: Int -> Int
hyperCeiling x = 2 ^ (ceiling . logBase 2 $ fromIntegral x)

mod' :: Integral a => a -> a -> a
mod' k n
  | m < 0 = m + n
  | otherwise = m
  where m = mod k n

modulo :: Int
modulo = 998244353

md :: Int -> Int
md x = x `mod` modulo

madd :: Int -> Int -> Int
madd x y = (x+y) `mod` modulo

-- https://rosettacode.org/wiki/Modular_inverse
modInv :: Int -> Int -> Maybe Int
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

eps :: Double
eps = 10**(-6)

newton :: (Double -> Double) -> (Double -> Double) -> Double -> Double
newton f f' x0 = improve x0
  where improve x
          | abs (f x) < eps = x
          | otherwise       = improve $ x - (f x / f' x)

class Group a where
  gzero    :: a
  gplus    :: a -> a -> a
  ginverse :: a -> a

instance Num a => Group (a, a) where
  gzero                 = (0, 0)
  (a, b) `gplus` (c, d) = (a+c, b+d)
  ginverse (a, b)       = (-a, -b)

-- upper exclusive
-- each Tuple (l, r, d) represents a half open interval [l, r)
imos :: Group a => Int -> Int -> [(Int, Int, a)] -> [a]
imos lower upper = scanl1 gplus . Data.Foldable.toList . accumArray gplus gzero (lower, upper) . concatMap adds
  where adds (l, r, d) = [(l, d), (r, ginverse d)]

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f l r = if l < r && f l then go l r
                else error "bsearch: invalid arguments"
  where go l r
          | l+1 == r  = l
          | f m       = go m r
          | otherwise = go l m
          where m = (l+r) `div` 2
