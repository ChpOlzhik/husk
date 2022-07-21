{-
Module 6. Higher order functions.
    Examples:
        1) :t (+5)

        2)
            gt100 = (>100)
            gt100 4
            gt100 150
        3)
            mod2 = (`mod` 2)
            mod2' = (mod 2)
            -- Quick quiz: what is the difference?
            
-}

--    Exercises:
--    1)  Implement sorting function of list of tuples
mySort :: (a -> a -> Bool) -> [a] -> [a]
mySort comparator list = undefined

--    2)  Implement sum function:
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
        
--3)  Function max
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)
        