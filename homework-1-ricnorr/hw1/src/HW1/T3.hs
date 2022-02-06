module HW1.T3
  ( Tree (..)
  , tFromList
  , tdepth
  , tinsert
  , tmember
  , tsize
  ) where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                  = 0
tsize (Branch (a, _) _ _ _) = a

tdepth :: Tree a -> Int
tdepth Leaf                  = 0
tdepth (Branch (_, a) _ _ _) = a

maxHeight :: Tree a -> Tree a -> Int
maxHeight l r = max (tdepth l) (tdepth r)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l val r = Branch (tsize l + tsize r + 1, maxHeight l r + 1) l val r

tLeft :: Tree a -> Tree a
tLeft (Branch _ left _ _) = left
tLeft Leaf                = error "tLeft on Leaf"

tRight :: Tree a -> Tree a
tRight (Branch _ _ _ right) = right
tRight Leaf                 = error "tRight on Leaf"

tValue :: Tree a -> a
tValue (Branch _ _ v _) = v
tValue Leaf             = error "tValue on Leaf"

rr :: Tree a -> Tree a
rr y = mkBranch (tLeft x) (tValue x) (mkBranch t2 (tValue y) (tRight y))
  where
    x = tLeft y
    t2 = tRight x

lr :: Tree a -> Tree a
lr x = mkBranch (mkBranch (tLeft x) (tValue x) t2) (tValue y) (tRight y)
  where
    y = tRight x
    t2 = tLeft y

rotate :: Ord a => Tree a -> a -> Tree a
rotate tree key
  | balance > 1 && key < tValue left = rr (mkBranch left val right)
  | balance < -1 && key > tValue right = lr (mkBranch left val right)
  | balance > 1 && key > tValue left = rr (mkBranch (lr left) val right)
  | balance < -1 && key < tValue right = lr (mkBranch left val (rr right))
  | otherwise = mkBranch left val right
  where
    balance = tdepth left - tdepth right
    left = tLeft tree
    right = tRight tree
    val = tValue tree

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ left value right)
  | x == value = True
  | x < value = tmember x left
  | x > value = tmember x right
tmember _ _ = undefined

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = Branch (1, 1) Leaf x Leaf
tinsert x (Branch _ left value right)
  | x == value = rotate (mkBranch left value right) x
  | x < value = rotate (mkBranch left' value right) x
  | x > value = rotate (mkBranch left value right') x
  where
    left' = tinsert x left
    right' = tinsert x right
tinsert _ _ = undefined

tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf
