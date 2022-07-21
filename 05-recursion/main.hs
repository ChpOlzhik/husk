{-
Examples:
        --Sum
        sum' :: [Integer] -> Integer
        sum' []     = 0
        sum' (x:xs) = x + sum' xs

        --Product
        product' :: [Integer] -> Integer
        product' [] = 1
        product' (x:xs) = x * product' xs

        --Length
        length' :: [a] -> Int
        length' []     = 0
        length' (_:xs) = 1 + length' xs
        
        -- Maximum
        maximum' :: Ord a => [a] -> a
        maximum' [] = error "maximum of empty list"
        maximum' [x] = x
        maximum' (x:xs)
                | x > maxTail = x
                | otherwise = maxTail
                where maxTail = maximum' xs

        -- LastElement
        lastElement :: Ord a => [a] -> a
        lastElement [] = error "last not exist"
        lastElement [x] = x
        lastElement (x:xs) = lastElement xs

        -- Palindrome
        isPalindrome :: Ord a => [a] -> Bool
        isPalindrome [] = error "Empty list"
        isPalindrome [x] = True
        isPalindrome xs = (head xs) == (last xs) && (isPalindrome (init (tail xs)))

        -- Replicate
        replicate' :: (Num i, Ord i) => i -> a -> [a]
        replicate' n x
            | n<=0 =[]
            | otherwise = x:replicate' (n-1) x

        quicksort :: (Ord a) => [a] -> [a]
        quicksort [] = []
        quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
            where smallerSorted = quicksort [a | a <- xs, a <= x]
                  biggerSorted = quicksort [a | a <- xs, a > x]

    Exercises:
        1) Write a function that reverses a list. [1,2,3] -> [3,2,1]
        2) Write a function that checks if the number is power of k. 
            isPowerOf n k = undefined
        3) Write a function that takes list and number i and drops every ith element from list. [1,2,3,4] 2 -> [1,3]
        4) Implement binary powering: 
            binpow k x = undefined
        5) First Nth of the fibonacci sequence. 10 -> 0 1 1 2 3 5 8 13 21 34;
-}

-- reverseList
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ x

-- isPowerOf
isPowerOf :: Int -> Int -> Bool
isPowerOf x y = undefined

-- dropEvery
dropEvery :: Int -> [a] -> [a]
dropEvery i xs = undefined

-- fibbonacci
fibbonacci :: Int -> Int
fibbonacci 0 = 0
fibbonacci 1 = 1
fibbonacci n = fibbonacci n-1 + fibbonacci n-2

