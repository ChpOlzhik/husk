{-
    Creating a zipper for a binary tree.

    Zippers are a purely functional way of navigating within a data structure and manipulating it. They essentially contain a data structure and a pointer into that data structure (called the focus).

    For example given a rose tree (where each node contains a value and a list of child nodes) a zipper might support these operations:

    from_tree (get a zipper out of a rose tree, the focus is on the root node)
    to_tree (get the rose tree out of the zipper)
    value (get the value of the focus node)
    prev (move the focus to the previous child of the same parent, returns a new zipper)
    next (move the focus to the next child of the same parent, returns a new zipper)
    up (move the focus to the parent, returns a new zipper)
    set_value (set the value of the focus node, returns a new zipper)
    insert_before (insert a new subtree before the focus node, it becomes the prev of the focus node, returns a new zipper)
    insert_after (insert a new subtree after the focus node, it becomes the next of the focus node, returns a new zipper)
    delete (removes the focus node and all subtrees, focus moves to the next node if possible otherwise to the prev node if possible, otherwise to the parent node, returns a new zipper)
-}

module Zipper (
    BinTree(..),
    Zipper,

    fromTree,
    toTree,
    
    value,
    left,
    right,
    up,

    setValue,
    setLeft,
    setRight) where

import Data.Maybe

-- | A binary tree.
data BinTree a = BT { 
    btValue :: a                 -- ^ Value
  , btLeft  :: Maybe (BinTree a) -- ^ Left child
  , btRight :: Maybe (BinTree a) -- ^ Right child
} deriving (Eq, Show)

data Direction = ThrowLeft | ThrowRight deriving (Eq, Show)

data Prev a = Prev { direction :: Direction
                   , parent    :: a
                   , sibling   :: Maybe (BinTree a)
                   }
                   deriving (Eq, Show)

-- | A zipper for a binary tree.
--   Zipper is a path to the focused node from the root node and subtree under focused node.
--   Using path we can go up, and using subtree we can go down (downleft or downright).
--   Path is a stack of passed tree nodes from top to currentSubtree with additional notes,
--   it's a:
--   - direction passed
--   - element in node
--   - sibling subtree
data Zipper a = Z { currentSubtree :: BinTree a
                  , path           :: [Prev a]
                  }
                  deriving (Eq, Show)

-- | Get a zipper focussed on the root node.
fromTree :: BinTree a -> Zipper a
fromTree tree = (Z tree [])

-- | Get the complete tree from a zipper.
toTree :: Zipper a -> BinTree a
toTree = currentSubtree . goTop
  where
    goTop z = maybe z goTop (up z)

-- | Get the value of the focus node.
value :: Zipper a -> a
value = btValue . currentSubtree

-- | Get the left child of the focus node, if any.
left :: Zipper a -> Maybe (Zipper a)
left (Z (BT currentValue nextLeft nextRight) prevPath) =
    goToDir ThrowLeft currentValue prevPath nextLeft nextRight

-- | Get the right child of the focus node, if any.
right :: Zipper a -> Maybe (Zipper a)
right (Z (BT currentValue nextLeft nextRight) prevPath) =
    goToDir ThrowRight currentValue prevPath nextRight nextLeft

goToDir :: Direction-> a -> [Prev a] -> Maybe (BinTree a) -> Maybe (BinTree a) -> Maybe (Zipper a)
goToDir dir currentValue prevPath next sibling
  | isNothing next = Nothing
  | otherwise =
    Just $ Z (fromJust next) ((Prev dir currentValue sibling):prevPath)

-- | Get the parent of the focus node, if any.
up :: Zipper a -> Maybe (Zipper a)
up (Z _               []) =
  Nothing
up (Z currentSubtree (Prev dir parent sibling:prev)) =
  Just $ Z (parentTree dir) prev
  where
    parentTree ThrowLeft  = BT parent (Just currentSubtree) sibling
    parentTree ThrowRight = BT parent sibling (Just currentSubtree)

-- | Set the value of the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue newValue (Z (BT _ left right) path) =
  Z (BT newValue left right) path

-- | Replace a left child tree.

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft left (Z (BT value _ right) path) =
  Z (BT value left right) path

-- | Replace a right child tree.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight right (Z (BT value left _) path) =
  Z (BT value left right) path