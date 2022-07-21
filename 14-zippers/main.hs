data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type BreadCrumbs a = [Crumb a]
type Zipper a = (Tree a, BreadCrumbs a)

x -: f = f x

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing 

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goRight (Empty, _) = Nothing 

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing