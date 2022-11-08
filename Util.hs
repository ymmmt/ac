-- IO

yn :: Bool -> String
yn True  = "Yes"
yn False = "No"

joinStr :: Show a => String -> [a] -> String
joinStr sep xs = intercalate sep $ map show xs

-- List

sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . group . sort

pairs1 :: [a] -> [(a, a)]
pairs1 []     = []
pairs1 [x]    = []
pairs1 (x:xs) = [(x, y) | y <- xs]

pairs :: [a] -> [(a, a)]
pairs = concatMap pairs1 . tails

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
