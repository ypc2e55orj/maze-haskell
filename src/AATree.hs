{-# OPTIONS -Wall #-}
module AATree
  ( AATree,
    toList,
    fromList,
    search,
    AATree.min,
    AATree.max,
    edit,
    insert,
    delete,
  )
where

-- 参考:
-- https://ja.wikipedia.org/wiki/AA%E6%9C%A8
-- https://www.cs.umd.edu/class/fall2019/cmsc420-0201/Lects/lect06-aa.pdf

-- 勉強のために自分で実装
-- データ構造が必要ならData.Mapなどを使うべき

data AATree k v
  = Node
      { getLevel :: Int,
        getKey :: k,
        getValue :: v,
        getLeft :: (AATree k v),
        getRight :: (AATree k v)
      }
  | Nil
  deriving (Show)

nil :: AATree k v -> Bool
nil Nil = True
nil _ = False

leaf :: AATree k v -> Bool
leaf (Node _ _ _ Nil Nil) = True
leaf _ = False

swapRight :: AATree k v -> AATree k v -> AATree k v
swapRight Nil _ = Nil
swapRight (Node level key value left _) right =
  Node level key value left right

swapLeft :: AATree k v -> AATree k v -> AATree k v
swapLeft Nil _ = Nil
swapLeft (Node level key value _ right) left =
  Node level key value left right

swapContents :: AATree k v -> AATree k v -> AATree k v
swapContents (Node level _ _ left right) (Node _ key value _ _) =
  Node level key value left right
swapContents _ _ = Nil

applyLevel :: (Int -> Int) -> AATree k v -> AATree k v
applyLevel _ Nil = Nil
applyLevel f (Node level key value left right) =
  Node (f level) key value left right

toList :: AATree k v -> [(k, v)]
toList Nil = []
toList (Node _ key value l r) =
  toList l ++ [(key, value)] ++ toList r

search :: Ord k => k -> AATree k v -> Maybe v
search _ Nil = Nothing
search fk (Node _ key value left right)
  | fk < key = search fk left
  | fk > key = search fk right
  | otherwise = Just value

min :: AATree k v -> AATree k v
min Nil = Nil
min tree@(Node _ _ _ Nil _) = tree
min (Node _ _ _ left _) = AATree.min left

max :: AATree k v -> AATree k v
max Nil = Nil
max tree@(Node _ _ _ _ Nil) = tree
max (Node _ _ _ _ right) = AATree.max right

edit :: Ord k => k -> v -> AATree k v -> AATree k v
edit _ _ Nil = Nil
edit ek v tree@(Node level key _ left right)
  | ek < key = swapLeft tree $ edit ek v left
  | ek > key = swapRight tree $ edit ek v right
  | otherwise = Node level key v left right

skew :: AATree k v -> AATree k v
skew Nil = Nil
skew tree@(Node level _ _ left _)
  | nil left = tree
  | getLevel left == level =
    swapRight left . swapLeft tree $ getRight left
  | otherwise = tree

split :: AATree k v -> AATree k v
split Nil = Nil
split tree@(Node level _ _ _ right)
  | nil right = tree
  | nil $ getRight right = tree
  | level == getLevel (getRight right) =
    increaseLevel . swapLeft right . swapRight tree $ getLeft right
  | otherwise = tree
  where
    increaseLevel = applyLevel (+ 1)

insert :: Ord k => (k, v) -> AATree k v -> AATree k v
insert (k, v) Nil = Node 1 k v Nil Nil
insert kv@(k, _) tree@(Node _ key _ left right)
  | k < key = f . swapLeft tree $ insert kv left
  | k > key = f . swapRight tree $ insert kv right
  | otherwise = error "AATree: Duplicate key"
  where
    f = split . skew

fromList :: Ord k => [(k, v)] -> AATree k v
fromList = foldr (\kv acc -> insert kv acc) Nil

decreaseLevel :: AATree k v -> AATree k v
decreaseLevel Nil = Nil
decreaseLevel tree@(Node _ _ _ left right)
  | leaf tree = tree
  | nil right = applyIdealLevel tree $ (getLevel left) + 1
  | nil left = applyIdealLevel tree $ (getLevel right) + 1
  | otherwise =
    let idealLevel = (Prelude.min (getLevel right) $ getLevel left) + 1
     in swapRight (applyIdealLevel tree idealLevel) $ applyIdealLevel right idealLevel
  where
    applyIdealLevel t idealLevel =
      if idealLevel < getLevel t
        then applyLevel (const idealLevel) t
        else t

successor :: AATree k v -> AATree k v
successor Nil = Nil
successor (Node _ _ _ _ right) = AATree.min right

predecessor :: AATree k v -> AATree k v
predecessor Nil = Nil
predecessor (Node _ _ _ left _) = AATree.max left
  where

delete :: Ord k => k -> AATree k v -> AATree k v
delete _ Nil = Nil
delete dk tree@(Node _ key _ left right)
  | dk < key = f . swapLeft tree $ delete dk left
  | dk > key = f . swapRight tree $ delete dk right
  | leaf tree = Nil
  | nil left =
    let s = successor tree
     in f . swapRight (swapContents tree s) $ delete (getKey s) right
  | otherwise =
    let p = predecessor tree
     in f . swapLeft (swapContents tree p) $ delete (getKey p) left
  where
    f t =
      let skewTree = skew . decreaseLevel $ t
          skewRight = skew $ getRight skewTree
          splitTree =
            split . swapRight skewTree . swapRight skewRight . skew $ getRight skewRight
       in swapRight splitTree . split $ getRight splitTree
