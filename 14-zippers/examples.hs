--------------------------------------------------------------------
-- Taking a walk
--------------------------------------------------------------------
{-
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = 
  Node 'P'  
    (Node 'O'  
      (Node 'L'  
        (Node 'N' Empty Empty)  
        (Node 'T' Empty Empty)  
      )  
      (Node 'Y'  
        (Node 'S' Empty Empty)  
        (Node 'A' Empty Empty)  
      )  
    )  
    (Node 'L'  
        (Node 'W'  
          (Node 'C' Empty Empty)  
          (Node 'R' Empty Empty)  
        )  
        (Node 'A'  
          (Node 'A' Empty Empty)  
          (Node 'C' Empty Empty)  
        )  
    )

-- changeToP :: Tree Char -> Tree Char 
-- changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char 
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

type BreadCrumbs a = [Crumb a]

goLeft :: (Tree a, BreadCrumbs a) -> (Tree a, BreadCrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, BreadCrumbs a) -> (Tree a, BreadCrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, BreadCrumbs a) -> (Tree a, BreadCrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Zipper a = (Tree a, BreadCrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost (goUp z)
-}

--------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------
{-
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)
-}

--------------------------------------------------------------------
-- File system
--------------------------------------------------------------------
{-
import Data.List (break)

type Name = String
type Data = String 
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = 
  Folder "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa"  
    , File "pope_time.avi" "god bless"  
    , Folder "pics"  
      [ File "ape_throwing_up.jpg" "bleargh"  
      , File "watermelon_smash.gif" "smash!!"  
      , File "skull_man(scary).bmp" "Yikes!"  
      ]  
    , File "dijon_poupon.doc" "best mustard"  
    , Folder "programs"  
      [ File "fartwizard.exe" "10gotofart"  
      , File "owl_bandit.dmg" "mov eax, h00t"  
      , File "not_a_virus.exe" "really not a virus"  
      , Folder "source code"  
        [ File "best_hs_prog.hs" "main = print (fix error)"  
        , File "random.hs" "main = print 4"  
        ]  
      ]  
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe FSZipper
fsUp (item, FSCrumb name ls rs:bs) = Just (Folder name (ls ++ [item] ++ rs), bs)
fsUp (item, []) = Nothing

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) = 
  let (ls, item:rs) = break (nameIs name) items
  in  (item, FSCrumb folderName ls rs: bs)

nameIs :: Name -> FSItem -> Bool 
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

x -: f = f x

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = 
  (Folder folderName (item:items), bs)
-}
--------------------------------------------------------------------
-- Watch your step
--------------------------------------------------------------------
{-
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
-}
