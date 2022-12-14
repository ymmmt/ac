-- Strict pair
-- Data.Tuple.Strict

data Pair a b = P !a !b

pfst :: Pair a b -> a
pfst (P x _) = x

psnd :: Pair a b -> b
psnd (P _ y) = y

-- Monad

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z []     = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

-- Data.Vector.Fusion.Stream.Monadic

rep :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
rep l r = flip FSM.mapM_ (stream l r)

stream :: (Monad m) => Int -> Int -> FSM.Stream m Int
stream !l !r = FSM.Stream step l
  where
    step x
      | x < r     = return $ FSM.Yield x (x + 1)
      | otherwise = return FSM.Done

-- IO

yn :: Bool -> String
yn True  = "Yes"
yn False = "No"

joinStr :: Show a => String -> [a] -> String
joinStr sep = intercalate sep . map show

-- Random

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt xy = state (randomR xy)

type Probability = Double

judgeSt :: Probability -> State StdGen Bool
judgeSt p = (< p) <$> randomRSt (0.0, 1.0)

-- Char Matrix

type Cell     = (Int, Int)
type Matrix a = Array Cell a
type CMatrix  = Matrix Char

readCMatrix :: Int -> Int -> IO CMatrix
readCMatrix h w = listArray ((1, 1), (h, w)) . concat <$> replicateM h getLine

printCMatrix :: CMatrix -> IO ()
printCMatrix m = mapM_ printRow rs
  where
    ((x, y), (z, w)) = bounds m
    rs               = range (x, z)
    cs               = range (y, w)
    printRow r       = mapM_ (putChar . (m!)) [(r, j) | j <- cs] >> putStrLn ""

adjacents :: CMatrix -> Cell -> [Cell]
adjacents c (i, j)
  | c!(i, j) == '#' = []
  | otherwise       = filter ((&&) <$> inRange (bounds c) <*> (/= '#') . (c!))
                      [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

encode :: Int -> Cell -> G.Vertex
encode w (i, j) = w * (i - 1) + j

edges :: CMatrix -> [(Cell, Cell)]
edges c = concatMap (\ij -> map (ij,) $ adjacents c ij) $ indices c

encodeEdges :: Int -> [(Cell, Cell)] -> [G.Edge]
encodeEdges w = map (cross (encode w, encode w))

-- Matrix

type Cell     = (Int, Int)
type Matrix a = Array Cell a

printMatrix :: Show a => String -> Matrix a -> IO ()
printMatrix s m = mapM_ printRow rs
  where
    ((x, y), (z, w)) = bounds m
    rs               = range (x, z)
    cs               = range (y, w)
    printRow r       = putStrLn . joinStr s $ map (m!) [(r, j) | j <- cs]

rowCells :: (Int, [a]) -> [(a, Cell)]
rowCells (i, xs) = map (\(j, x) -> (x, (i, j))) $ zip [1..] xs

rectSum :: Num b => (a -> b) -> Matrix a -> (Cell -> Cell -> b)
rectSum f m = get
  where
    ((1, 1), (h, w)) = bounds m
    s = listArray ((0, 0), (h, w)) $ build 1 [replicate (w+1) 0]
    build r xss@(xs:_)
      | r > h     = concat (reverse xss)
      | otherwise = build (r+1) (ys:xss)
      where ys = zipWith (+) xs $ scanl' (+) 0 [f (m!(r, c)) | c <- [1..w]]
    get (i, j) (k, l)
      | i <= k && j <= l = s!(k, l) - s!(k, j-1) - s!(i-1, l) + s!(i-1, j-1)

-- Converts row major index to cell
cell :: Int -> Int -> Cell
cell w i = (q+1, r+1)
  where (q, r) = divMod (i-1) w

-- Row major index: (1, 1) <--> 1, (h, w) <--> h*w
rmi :: Int -> Cell -> Int
rmi w (i, j) = (i-1)*w + j
{-# INLINE rmi #-}

-- Tuple

tupWith :: (a -> b) -> a -> (a, b)
tupWith f x = (x, f x)

-- MArray (for Graph)

see :: (MArray a Bool m, Ix i) => a i Bool -> i -> m ()
see marr i = writeArray marr i True

notSeen :: (MArray a Bool m, Ix i) => a i Bool -> i -> m Bool
notSeen marr i = not <$> readArray marr i

-- MArray

update :: (Num e, MArray a e m, Ix i) => (e -> e -> e) -> a i e -> i -> e -> m ()
update f marr i y = do
  x <- readArray marr i
  writeArray marr i (f x y)

-- Array

tabulate :: Ix i => (i -> e) -> (i, i) -> Array i e
tabulate f bounds = array bounds [(x, f x) | x <- range bounds]

-- Vector

readCVector :: Int -> Int -> Int -> IO (UV.Vector Char)
readCVector h w k
  = UV.unsafeAccum (flip const) (UV.replicate (h*w+1) ' ')
    <$> replicateM k (do
                         [sh, sw, sc] <- words <$> getLine
                         return (rmi w (read sh, read sw), head sc))

-- List

unwrap :: [a] -> a
unwrap [x] = x

single :: [a] -> Bool
single [x] = True
single _   = False

same :: Eq a => [a] -> Bool
same []     = True
same (x:xs) = all (== x) xs

type Transposition = (Int, Int)

swap :: Transposition -> [a] -> [a]
swap (i, j) a
  | i == j    = a
  | otherwise = take k a ++ [a!!l] ++ drop (k+1) (take l a) ++ [a!!k] ++ drop (l+1) a
  where k = min i j
        l = max i j

sg :: Ord a => [a] -> [[a]]
sg = group . sort

sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . sg

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . sg

repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1)

repeatedBy :: Ord a => (Int -> Bool) -> [a] -> [a]
repeatedBy p = map head . filterByLength p

allUnique :: Ord a => [a] -> Bool
allUnique = all (null . tail) . sg

diff :: Ord a => [a] -> [a] -> [a]
diff xs [] = xs
diff [] _  = []
diff xs@(x:xs') ys@(y:ys')
  | x < y     = x:diff xs' ys
  | x > y     = diff xs ys'
  | otherwise = diff xs' ys'

intersect :: Ord a => [a] -> [a] -> [a]
intersect xs [] = []
intersect [] ys = []
intersect xs@(x:xs') ys@(y:ys')
  | x < y     = intersect xs' ys
  | x > y     = intersect xs ys'
  | otherwise = x:intersect xs' ys'

coordComp :: Ord a => Int -> [a] -> M.Map Int a
coordComp i0 = M.fromAscList . zip [i0..] . sortUniq

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- psSums = sort . map sum . powerset
psSums :: (Ord a, Num a) => [a] -> [a]
psSums []     = [0]
psSums (x:xs) = merge ys $ map (+x) ys
  where ys = psSums xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> x:merge xs' ys
    EQ -> x:y:merge xs' ys'
    GT -> y:merge xs ys'

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | x:ys <- tails xs, y:zs <- tails ys]

choices :: Int -> [a] -> [[a]]
choices 0 _   = [[]]
choices _ []  = []
choices n (x:xs)
  | n > 0     = (map (x:) $ choices (n-1) xs) ++ choices n xs
  | otherwise = []

bfilter :: [a] -> [Bool] -> [a]
bfilter xs bs = map fst . filter snd $ zip xs bs

minimumOn :: Ord b => (a -> b) -> [a] -> (a, b, Int)
minimumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx <= ky then u else v

maximumOn :: Ord b => (a -> b) -> [a] -> (a, b, Int)
maximumOn k xs = foldl1 step $ zip3 xs (map k xs) [0..]
  where step u@(_, kx, _) v@(_, ky, _) =
          if kx >= ky then u else v

sumOn :: Num b => (a -> b) -> [a] -> b
sumOn f = foldl' step 0
  where step s x = s + f x

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

positions :: Ix a => [a] -> Array a [Int]
positions (x:xs) = accumArray (flip (:)) [] (l, u) $ zip ys [n, n-1..1]
  where
    (l, u, n, ys)        = foldl' step (x, x, 1, [x]) xs
    step (l, u, n, ys) y = (min l y, max u y, n+1, y:ys)

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

scanlWithIndex' :: (a -> b -> Int -> a) -> a -> Int -> [b] -> [a]
scanlWithIndex' f e i []     = [e]
scanlWithIndex' f e i (x:xs) = e `seq` e:scanlWithIndex' f (f e x i) (i+1) xs

interleave :: [a] -> [a] -> [a]
interleave xs     []     = xs
interleave []     ys     = ys
interleave (x:xs) (y:ys) = x:y:interleave xs ys

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = ys:chunks n zs
  where (ys, zs) = splitAt n xs

inserts :: a -> [a] -> [[a]]
inserts x []     = [[x]]
inserts x (y:ys) = (x:y:ys):map(y:) (inserts x ys)

perms :: [a] -> [[a]]
perms = foldr step [[]]
  where
    step x xss = concatMap (inserts x) xss

type Partition a = [Segment a]
type Segment a   = [a]

splits :: [a] -> [(Segment a, Segment a)]
splits []     = []
splits (x:xs) = ([x], xs):[(x:ys, zs) | (ys, zs) <- splits xs]

parts :: [a] -> [Partition a]
parts [] = [[]]
parts xs = [ys:yss | (ys, zs) <- splits xs, yss <- parts zs]

-- Map

counter :: Ord a => [a] -> M.Map a Int
counter = foldr (\x mp -> M.insertWith f x 1 mp) M.empty
  where f _ old = old + 1

-- Set

intervalMember :: (Ord a, Bounded a) => a -> a -> S.Set a -> Bool
intervalMember l r s = x <= y
  where x = fromMaybe maxBound $ S.lookupGE l s
        y = fromMaybe minBound $ S.lookupLE r s

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

apply :: Int -> (a -> a) -> a -> a
apply 0 _ = id
apply n f = f . apply (n-1) f

-- Graph

type Size     = Int
type Depth    = Int
type Height   = Int
type Distance = Int

data Tree a = Node { rootLabel :: a
                   , subForest :: [Tree a]
                   , parent    :: Maybe (Tree a)
                   , size      :: Size
                   , depth     :: Depth
                   , height    :: Height }

instance Show a => Show (Tree a) where
  show t = "Node {rootLabel = " ++ (show $ rootLabel t)
           ++ ", subForest = " ++ show (map rootLabel $ subForest t)
           ++ ", parent = " ++ (show $ rootLabel <$> parent t)
           ++ ", size = " ++ (show $ size t)
           ++ ", depth = " ++ (show $ depth t)
           ++ ", height = " ++ (show $ height t) ++ "}"

instance Eq a => Eq (Tree a) where
  (==) = (==) `on` rootLabel

-- needed to simplify lpd algorithm
ensureHighestFirst :: [Tree G.Vertex] -> [Tree G.Vertex]
ensureHighestFirst [] = []
ensureHighestFirst (t:ts) = t':ts'
  where (t', ts') = foldl step (t, []) ts
        step (ht, acc) t
          | height ht < height t = (t, ht:acc)
          | otherwise            = (ht, t:acc)

buildT :: G.Graph -> G.Vertex -> Tree G.Vertex
buildT g r = go 0 Nothing (-1) r
  where
    go d p u v = n
      where n  = Node v ts p s d h
            ts = ensureHighestFirst . map (go (d+1) (Just n) v)
                 $ filter (/= u) (g!v)
            s  = 1 + (sum $ map size ts)
            h  = 1 + (if null ts then 0 else height $ head ts)

rebuild :: T.Tree a -> Tree a
rebuild = go 0 Nothing
  where
    go d p (T.Node v []) = Node v [] p 1 d 1
    go d p (T.Node v ts) = n
      where ts' = sortOn (negate . height) $ map (go (d+1) (Just n)) ts
            s   = 1 + (sum $ map size ts')
            h   = 1 + (height $ head ts')
            n   = Node v ts' p s d h

dfs :: G.Graph -> G.Vertex -> Tree G.Vertex
dfs g r = rebuild . head $ G.dfs g [r]

buildUndirectedG :: G.Bounds -> [G.Edge] -> G.Graph
buildUndirectedG b = G.buildG b . concatMap (\(u, v) -> [(u, v), (v, u)])

dists :: G.Graph -> G.Vertex -> Array G.Vertex Int
dists g r = array (bounds g) [(v, d) | (vs, d) <- zip (T.levels t) [0..], v <- vs]
  where
    t = head (G.dfs g [r])

leaves :: G.Graph -> [G.Vertex]
leaves = map fst . filter ((== 1) . snd) . assocs . G.indegree

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

graphSize :: G.Graph -> Size
graphSize g = rangeSize (bounds g)

clique :: G.Graph -> [G.Vertex] -> Bool
clique g vs = all adj $ choices 2 vs
  where adj [v, w] = v `elem` g!w

dfsPath :: G.Graph -> G.Vertex -> G.Vertex -> Maybe [G.Vertex]
dfsPath g s t = go (-1) [s]
  where
    go p vs@(v:_)
      | v == t    = Just (reverse vs)
      | null cs   = Nothing
      | otherwise = asum $ map (go v . (:vs)) cs
      where cs = delete p (g!v)

bipartite :: G.Graph -> G.Vertex -> Maybe ([G.Vertex], [G.Vertex])
bipartite g s = dfs (S.singleton s) (S.empty) [(s, True)]
  where
    dfs e o []              = Just (S.toList e, S.toList o)
    dfs e o ((v, even):ves) = dfs e' o' . (++ ves) . map (, odd) =<< vs'
      where
        odd = not even
        e'  = if odd  then foldr S.insert e (fromJust vs') else e
        o'  = if even then foldr S.insert o (fromJust vs') else o
        vs' = foldr new (Just []) (g!v)
        new _ Nothing = Nothing
        new w m@(Just ws)
          | (even && S.member w e) || (odd && S.member w o) = Nothing
          | (even && S.member w o) || (odd && S.member w e) = m
          | otherwise                                       = Just (w:ws)

-- Bit set

type BitSet = Int
type Size   = Int
type Member = Int

bsUniv :: Size -> BitSet
bsUniv n = 2^n - 1

bsSize :: BitSet -> Size
bsSize b = finiteBitSize b - 1

bsCount :: BitSet -> Int
bsCount = length . bsMembers

bsEmpty :: BitSet -> Bool
bsEmpty = (== 0)

bsSingleton :: Member -> BitSet
bsSingleton = flip bsInsert 0

bitset :: [Member] -> BitSet
bitset = foldr bsInsert 0

bsMember :: Member -> BitSet -> Bool
bsMember = flip testBit

bsMembers :: BitSet -> [Member]
bsMembers b = filter (flip bsMember b) [0..bsSize b]

bsDiff :: BitSet -> BitSet -> BitSet
bsDiff b b' = foldl' clearBit b (bsMembers b')

bsInsert :: Member -> BitSet -> BitSet
bsInsert = flip setBit

bsDelete :: Member -> BitSet -> BitSet
bsDelete x b = bsDiff b (bsSingleton x)

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

c2 :: Int -> Int
c2 n = if n >= 2 then n*(n-1)`div`2 else 0

tri :: Int -> Int
tri n = n * (n+1) `div` 2

cut :: Ord a => a -> a -> a -> a
cut l h | l > h     = undefined
        | otherwise = max l . min h

hyperFloor :: Int -> Int
hyperFloor x = 2 ^ (floor . logBase 2 $ fromIntegral x)

hyperCeiling :: Int -> Int
hyperCeiling x = 2 ^ (ceiling . logBase 2 $ fromIntegral x)

floor' :: Int -> Int -> Int
floor' x y = floor (fromIntegral x / fromIntegral y)

ceiling' :: Int -> Int -> Int
ceiling' x y = ceiling (fromIntegral x / fromIntegral y)

-- make smallest prime factors vector
mkspfv :: Int -> Array Int Int
mkspfv n
  | n >= 1 = array (1, n) $ concatMap mulAssocs [n, n-1..2] ++ [(1, 1)]
  where mulAssocs k = [(d, k) | d <- [k, 2*k..n]]

-- prime factorization with spfv
pfactors :: Array Int Int -> Int -> [Int]
pfactors _    1 = []
pfactors spfv n = p:pfactors spfv (n `div` p)
  where p = spfv!n

pfactors :: Int -> [Int]
pfactors n = case fs of
               []  -> [n]
               f:_ -> f:pfactors (n `div` f)
  where fs = take 1 $ filter ((== 0) . (n `mod`)) [2..n']
        n' = floor . sqrt $ fromIntegral n

divisors :: Int -> [Int]
divisors = foldr (concatMap . mulpowers) [1] . multiSet . pfactors
  where
    mulpowers (p, i) d = [d * p^j | j <- [0..i]]

mod' :: Integral a => a -> a -> a
mod' k n
  | m < 0 = m + n
  | otherwise = m
  where m = mod k n

modulo :: Int
modulo = 998244353

md :: Int -> Int
md x = x `mod` modulo

infixl 6 `madd`
madd :: Int -> Int -> Int
madd x y = (x+y) `mod` modulo

infixl 7 `mmul`
mmul :: Int -> Int -> Int
mmul x y = (x*y) `mod` modulo

modPow :: Int -> Int -> Int
modPow x 0 = 1
modPow x n = mmul x $ modPow x (n-1)

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

modFacts :: Int -> Array Int Int
modFacts n = fs
  where
    fs  = tabulate f (0, n)
    f n = if n <= 1 then 1 else n `mmul` fs!(n-1)

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

type Hash = Int

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

-- Batched Queue

data BatchedQueue a = BQ [a] [a]
  deriving (Show)

check :: [a] -> [a] -> BatchedQueue a
check [] r = BQ (reverse r) []
check f  r = BQ f r

empty :: BatchedQueue a
empty = BQ [] []

qnull :: BatchedQueue a -> Bool
qnull (BQ f _) = null f

qsingleton :: a -> BatchedQueue a
qsingleton x = BQ [x] []

snoc :: BatchedQueue a -> a -> BatchedQueue a
snoc (BQ f r) x = check f (x:r)

qhead :: BatchedQueue a -> a
qhead (BQ []    _) = error "empty queue"
qhead (BQ (x:f) r) = x

qtail :: BatchedQueue a -> BatchedQueue a
qtail (BQ []    _) = error "empty queue"
qtail (BQ (x:f) r) = check f r

quncons :: BatchedQueue a -> (a, BatchedQueue a)
quncons (BQ []    _) = error "empty queue"
quncons (BQ (x:f) r) = (x, check f r)

-- Dijkstra's algorithm

type Graph    = ([Vertex], [Edge])
type Edge     = (Vertex, Vertex, Weight)
type Vertex   = Int
type Weight   = Int
type Weights  = Array (Vertex, Vertex) Weight

type Tree     = Graph
type Forest   = [Tree]

type State    = (Links, [Vertex])
type Links    = Array Vertex (Vertex, Distance)
type Distance = Int

nodes :: Graph -> [Vertex]
nodes (vs, _) = vs

edges :: Graph -> [Edge]
edges (_, es) = es

source :: Edge -> Vertex
source (u, _, _) = u

target :: Edge -> Vertex
target (_, v, _) = v

weight :: Edge -> Weight
weight (_, _, w) = w

weights :: Graph -> Weights
weights g = listArray ((1, 1), (n, n)) (repeat maxInt)
            // [((u, v), w) | (u, v, w) <- edges g]
  where n = length (nodes g)

parent :: Links -> Vertex -> Vertex
parent ls v = fst (ls!v)

distance :: Links -> Vertex -> Distance
distance ls v = snd (ls!v)

extract :: State -> Tree
extract (ls, _) = (indices ls, [(u, v, w) | (v, (u, w)) <- assocs ls, v /= 1])

start :: Int -> State
start n = (array (1, n) ((1, (1, 0)):[(v, (v, maxInt)) | v <- [2..n]]), [1..n])

maxInt :: Int
maxInt = maxBound

gstep :: Weights -> State -> State
gstep wa (ls, vs) = (ls', vs')
  where
    (d, v) = minimum [(distance ls v, v) | v <- vs]
    vs'    = filter (/= v) vs
    ls'    = accum better ls [(u, (v, sum d (wa!(v, u)))) | u <- vs']
      where sum d w = if w == maxInt then maxInt else d + w
    better (v1, d1) (v2, d2) = if d1 <= d2 then (v1, d1) else (v2, d2)

dijkstra :: Graph -> Tree
dijkstra g = extract $ apply (n-1) (gstep wa) (start n)
  where n  = length (nodes g)
        wa = traceShowId $ weights g

dist :: Tree -> Vertex -> Vertex -> Maybe Distance
dist t s g = dfs 0 s
  where
    children = accumArray (flip (:)) [] (1, h*w) [(u, (v, w)) | (u, v, w) <- edges t]
    dfs d u
      | u == g    = Just d
      | null cs   = Nothing
      | otherwise = asum $ map go cs
      where cs        = children!u
            go (v, w) = dfs (d+w) v

-- Thinning

thinBy :: (a -> a -> Bool) -> [a] -> [a]
thinBy _ []  = []
thinBy _ [x] = [x]
thinBy cmp (x:y:xs)
  | cmp x y   = thinBy cmp (x:xs)
  | cmp y x   = thinBy cmp (y:xs)
  | otherwise = x:thinBy cmp (y:xs)

mergeBy :: (a -> a -> Bool) -> [[a]] -> [a]
mergeBy cmp = foldr merge []
  where
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | cmp x y   = x:merge xs (y:ys)
      | otherwise = y:merge (x:xs) ys

-- longest common subsequences
-- from ADwH
ext (p, k, ws, us) = us
psn (p, k, ws, us) = p
lng (p, k, ws, us) = k

cons x (p, k, ws, us) = (p-1-length as, k+1, tail bs, x:us)
  where (as, bs) = span (/= x) ws

lcs :: Eq a => [a] -> [a] -> [a]
lcs xs = ext . head . foldr tstep start
  where
    start              = [(length xs, 0, reverse xs, [])]
    tstep y yss        = thinBy (<|) (mergeBy cmp [yss, zss])
      where
        zss       = dropWhile negpos (map (cons y) yss)
        negpos ys = psn ys < 0
        q1 <| q2  = psn q1 >= psn q2 && lng q1 >= lng q2
        cmp q1 q2 = psn q1 <= psn q2

--
