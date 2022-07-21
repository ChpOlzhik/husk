-- logical operations

{-
    What is the logical operations in haskell ?
    Logical and, or, not
-}

-- Examples
-- Double num
doubleMe x = x + x

-- Double sum
doubleUs x y = doubleMe x + doubleMe y

-- Double small number
doubleSmallNumber x = if x > 100 then x else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- Remove Non Uppercase in String
removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

-- Length of array
length' xs = sum [1 | _ <- xs]


-- Exercises
-- valueOf at index O(n)
valueOf :: Int -> [a] -> Maybe a
valueOf ind (x:xs) = if ind == 0 then Just x else valueOf (ind-1) xs

-- Get coordinates from two lists xs, ys.
coordinates :: Num a => [a] -> [a] -> [(a,a)]
coordinates [] _ = []
coordinates _ [] = []
coordinates (x:xs) (y:ys) = (x, y):coordinates xs ys

