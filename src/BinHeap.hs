module BinHeap where

data Tree a = Node Int a [Tree a]
  deriving (Show, Eq)

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r + 1) x1 (t2 : c1)
  | otherwise = Node (r + 1) x2 (t1 : c2)

type Heap a = [Tree a]

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ a _) = a

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree x [] = [x]
insTree x (t:ts)
  | rank x < rank t = x : ts
  | otherwise = insTree (link x t) ts

insert :: Ord a => a -> Heap a -> Heap a
insert x = insTree (Node 0 x [])

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ts [] = ts
merge [] ts = ts
merge ts1@(x:xs) ts2@(y:ys)
  | rank x < rank y = x : merge xs ts2
  | rank x > rank y = y : merge ts1 ys
  | otherwise = insTree (link x y) (merge xs ys)

removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
removeMinTree [] = error "removeMinTree given empty list"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
  let (x, xs) = removeMinTree ts
  in if root t <= root x
    then (t, ts)
    else (x, t : xs)

findMin :: Ord a => [Tree a] -> a
findMin [] = error "findMin given empty list"
findMin [t] = root t
findMin (t:ts) =
  let x = findMin ts
  in if root t <= x then root t else x

deleteMin :: Ord a => [Tree a] -> Heap a
deleteMin ts =
  let (Node _ x xs, ys) = removeMinTree ts
  in merge (reverse xs) ys
