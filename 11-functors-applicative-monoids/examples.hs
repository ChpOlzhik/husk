{-

#Functors

-- instance Functor IO where  
--     fmap f action = do  
--         result <- action  
--         return (f result)  

main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

-}


{-
import Data.Char  
import Data.List  
  
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line  
-}


{- 
Law 1. 
The first functor law states that if we map the id function over a functor, the functor that we get back should be the same as the original functor.

instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing

Law 2.
The second law says that composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one.    


data CMaybe a = CNothing | CJust Int a deriving (Show) 

instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)

main  = do 
    putStrLn $ show res1
    putStrLn $ show res2
    putStrLn $ show res3
    where
        res1 = fmap (++"ha") (CJust 0 "ho")
        res2 = fmap (++"he") res1
        res3 = fmap (++"heho") CNothing
-}

{-
# Applicative functors
import Control.Applicative

Law 1.
pure f <*> x = fmap f x

Law 2.
pure id <*> v = v

Law 3.
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

Law 4.
pure f <*> pure x = pure (f x)

Law 5.
u <*> pure y = pure ($ y) <*> u
-}

{-
# newtype
-}

{-
# Monoids

import Data.Monoid

newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)  

instance Monoid Any where  
    mempty = Any False  
    Any x `mappend` Any y = Any (x || y) 

Law 1.
mempty `mappend` x = x

Law 2.
x `mappend` mempty = x

Law 3.
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)


Another example
import qualified Foldable as F  

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m  

instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  

let testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )  
main = do
    putStrLn & F.foldl (+) 0 testTree  

-}
