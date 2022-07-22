{-

------------------------------------  
    Apply maybe
------------------------------------
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing 
applyMaybe (Just x) f = f x

instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing  

------------------------------------  
    List monads
------------------------------------

listOfTuples :: [(Int,Char)]
listOfTuples = do
  n <- [1,2]
  ch <- ['a','b']
  return (n,ch)

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x

------------------------------------  
    Knigt Queens
------------------------------------
import Control.Monad

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
             ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
             ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start
------------------------------------


Monad laws

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
f <=< g = (\x -> g x >>= f) 

f x = [x, -x]
g x = [x*3, x*2]
h = f <=< g

Law 1.
return x >>= f is the same damn thing as f x

Law 2.
m >>= return is no different than just m


Law 3.
Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)


-}