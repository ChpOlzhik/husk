
middle :: (a, b, c) -> b
middle (_,a,_) = a 

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y


changeSecond :: [a] -> [a]
changeSecond [] = []
changeSecond (x:[]) = [x]
changeSecond (x:y:[]) = [y,x]
changeSecond (x:y:z) = (y:x:z)

isPowerOfTwo :: Integral a => a -> Bool
isPowerOfTwo a 
    | a <= 0 = False
    | a == 1 || a == 2 = True
    | (a `mod` 2) /= 0 = False
    | otherwise = isPowerOfTwo (a `div` 2)

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys