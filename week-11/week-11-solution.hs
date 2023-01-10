import Data.List

inOrder :: Int -> Int -> [Int]
inOrder a b
  | a > b = a:[b]
  | otherwise = b:[a]

inOrderTC :: (Ord a) => a -> a -> [a]
inOrderTC a b
  | a > b = a:[b]
  | otherwise = b:[a]

data Gender = Male | Female | NO
data CarType = Cabrio | Bus | Jeep
data Education = PRIMARY | SECONDARY | HIGHER
data TypeOfAnimal = AMPHIBIOUS | GROUNDBEEF | VEGAN
data Faculties = Physicas | Maths | TeRest

data BTree a = Nil | Node a (BTree a) (BTree a)
  deriving (Show, Eq, Read)

poopyTree = (Node 17 (Node 14 Nil (Node 7 Nil Nil)) (Node 20 Nil Nil))

poopyTreeBST = (Node 17 (Node 14 Nil (Node 16 Nil Nil)) (Node 20 Nil Nil))


depthT :: BTree a -> Int
depthT Nil = 0
depthT (Node _ l r) = 1 + max (depthT l) (depthT r)


countLeaves :: BTree a -> Int
countLeaves Nil = 0
countLeaves (Node _ l r) = 1 + countLeaves l + countLeaves r

equalT :: (Eq a) => BTree a -> BTree a -> Bool
equalT Nil Nil = True
equalT (Node _ l r) Nil = False
equalT Nil (Node _ l r) = False
equalT (Node a1 l1 r1) (Node a2 l2 r2) = a1 == a2 && equalT l1 l2 && equalT r1 r2

dfsT :: BTree a -> [a]
dfsT Nil = []
dfsT (Node x l r) =  dfsT l ++ [x] ++ dfsT r

-- isBST :: BTree a -> Bool
-- isBST Nil = True
-- isBST (Node _ Nil Nil) = True
-- isBST (Node x l@(Node y _ _) Nil) = x >= y && isBST l
-- isBST (Node x Nil r@(Node y _ _)) = x <= y && isBST r
-- isBst (Node x l@(Node lx _ _) r@(Node rx _ _)) =

isBSTYolo :: (Ord a ) =>  BTree a -> Bool
isBSTYolo t = (sort dfsTraversal) == dfsTraversal where
  dfsTraversal = dfsT t



mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node x l r) = Node x (mirrorTree r) (mirrorTree l)

--[(2,4), (7,8), (2,3), (4,5)] -> [(2,5), (7,8)]

--minimalSurface :: [(Int, Int)] -> [(Int, Int)]
