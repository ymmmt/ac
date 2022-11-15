-- IO

yn :: Bool -> String
yn True  = "Yes"
yn False = "No"

joinStr :: Show a => String -> [a] -> String
joinStr sep xs = intercalate sep $ map show xs

-- Char Matrix

type Cell    = (Int, Int)
type CMatrix = Array Cell Char

readCMatrix :: Int -> Int -> IO CMatrix
readCMatrix h w = do
  rs <- replicateM h getLine
  let as               = concatMap colAssocs $ zip [1..h] rs
      colAssocs (i, r) = map (cross ((i,), id)) $ zip [1..w] r
  return $ array ((1, 1), (h, w)) as

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

-- List

sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . group . sort

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

-- Math

qt :: Bool -> Int
qt True  = 1
qt False = 0

arithSeqSum :: Integral a => a -> a -> a -> a
arithSeqSum n d a0 = n * a0 + d * n * (n-1) `div` 2

cut :: Ord a => a -> a -> a -> a
cut l h | l > h     = undefined
        | otherwise = max l . min h

mod' :: Integral a => a -> a -> a
mod' k n
  | m < 0 = m + n
  | otherwise = m
  where m = mod k n

modulo :: Int
modulo = 998244353

md :: Int -> Int
md x = x `mod` modulo

modAdd :: Int -> Int -> Int
modAdd x y = (x+y) `mod` modulo

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

dot :: [Int] -> [Int] -> Int
dot xs ys = sum $ zipWith (*) xs ys
