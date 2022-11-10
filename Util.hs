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

arithSeqSum :: Integral a => a -> a -> a -> a
arithSeqSum n d a0 = n * a0 + d * n * (n-1) `div` 2
