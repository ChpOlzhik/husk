{-
import Data.Monoid

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

type Food = String 
type Price = Sum Int 

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)
-}

--------------------------------------------------------------------
-- Writer
--------------------------------------------------------------------

{-
import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int 
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a*b)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  DiffList f <> DiffList g = DiffList (\xs -> f (g xs))
instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    gcd' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x-1)
  tell (toDiffList [show x])

finalCountDownSlow :: Int -> Writer [String] ()
finalCountDownSlow 0 = do
  tell ["0"]
finalCountDownSlow x = do
  finalCountDownSlow (x-1)
  tell [show x]
-}
--------------------------------------------------------------------
-- Reader
--------------------------------------------------------------------

{-
import Control.Monad.Instances

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

addStuff2 :: Int -> Int
addStuff2 x = let
  a = (*2) x
  b = (+10) x
  in (a+b)

-- instance Monad ((->) r) where
--   return x = \_ -> x
--   h >>= f = \w -> f (h w)
-}

--------------------------------------------------------------------
-- State
--------------------------------------------------------------------
{-
import Control.Monad.State

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  ((), newStack1) = push 3 stack
  (a , newStack2) = pop newStack1
  in pop newStack2

stackManip2 = do
  push 3
  a <- pop
  pop

-- newtype State s a = State { runState :: s -> (a,s) }

-- instance Applicative (State s) => Monad (State s) where
--   return x = State $ \s -> (x,s)
--   (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                       (State g) = f a
--                                   in  g newState

popState :: State Stack Int
popState = state $ \(x:xs) -> (x,xs)

pushState :: Int -> State Stack ()
pushState a = state $ \xs -> ((), a:xs)

stackManipState :: State Stack Int
stackManipState = do
  pushState 3
  popState
  popState

stackStuff :: State Stack ()
stackStuff = do
  a <- popState
  if a == 5
    then pushState 5
    else do
      pushState 3
      pushState 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManipState
  if a == 100
    then stackStuff
    else return ()

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get 
  if stackNow == [1,2,3]
    then put [8,3,1]
    else put [9,2,1]
-}

--------------------------------------------------------------------
-- Random
--------------------------------------------------------------------

{-
import System.Random
import Control.Monad.State

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a,b,c)
-}

--------------------------------------------------------------------
-- Useful functions
--------------------------------------------------------------------

{-
import Control.Monad.Writer
import Control.Monad
import Data.List

-- liftM :: (Moand m) => (a -> b) -> m a -> m b
-- liftM f m = m >>= (\x -> return (f x))

-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f m = do
--   x <- m
--   return (f x)

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
  f <- mf
  x <- m
  return (f x)

join :: (Monad m) => m (m a) -> m a
join mm = do
  m <- mm
  m

joinMaybes :: Maybe Int
joinMaybes = do
  m <- Just (Just 8)
  m

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

binSmalls :: Int -> Int -> Maybe Int 
binSmalls acc x
  | x > 9 = Nothing 
  | otherwise = Just (acc + x)

solveRPN :: String -> Maybe Double 
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing
-}

--------------------------------------------------------------------
-- Making monads
--------------------------------------------------------------------
{-

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Ratio
import Data.List
import Control.Monad (ap)
import Data.Function

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [
    (Prob [('a',1%2),('b',1%2)], 1%4),
    (Prob [('c',1%2),('d',1%2)], 3%4)
  ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x,r) -> (x,p*r)) innerxs

instance Applicative Prob where
  pure = return
  (<*>) = ap

instance Applicative Prob => Monad Prob where
  return x = Prob [(x,1%1)]
  m >>= f = flatten (fmap f m)

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool 
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (==Tails) [a,b,c])

runProb :: Eq a => Prob a -> [(a,Rational)]
runProb = map (\x -> (fst (head x), sum (map snd x)))
          . groupBy ((==) `on` fst)
          . getProb
-}