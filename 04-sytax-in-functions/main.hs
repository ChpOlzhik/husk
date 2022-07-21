{-
Examples:
        1)
            lucky :: (Integral a) => a -> String  
            lucky 7 = "LUCKY NUMBER SEVEN!"  
            lucky x = "Sorry, you're out of luck, pal!" 
        2)  
            sayMe :: (Integral a) => a -> String  
            sayMe 1 = "One!"  
            sayMe 2 = "Two!"  
            sayMe 3 = "Three!"  
            sayMe 4 = "Four!"  
            sayMe 5 = "Five!"  
            sayMe x = "Not between 1 and 5"  
        3) 
            factorial :: (Integral a) => a -> a  
            factorial 0 = 1  
            factorial n = n * factorial (n - 1)  
        4) 
            charName :: Char -> String  
            charName 'a' = "Asan"  
            charName 'b' = "Bolat"  
            charName 'u' = "Usen"
        5)   
            addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
            addVectors a b = (fst a + fst b, snd a + snd b)  
        
            addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
            addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 
        6)
            first :: (a, b, c) -> a  
            first (x, _, _) = x  
              
            second :: (a, b, c) -> b  
            second (_, y, _) = y  
              
            third :: (a, b, c) -> c  
            third (_, _, z) = z  

        7)
            let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
            [a+b | (a,b) <- xs]
            [4,7,6,8,11,4]
        8) 
            head' :: [a] -> a  
            head' [] = error "Can't call head on an empty list, dummy!"  
            head' (x:_) = x  
        9)  
            tell :: (Show a) => [a] -> String  
            tell [] = "The list is empty"  
            tell (x:[]) = "The list has one element: " ++ show x  
            tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
            tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
        10)
            length' :: (Num b) => [a] -> b  
            length' [] = 0  
            length' (_:xs) = 1 + length' xs 
        11) 
            ghci> capital "Marat"  
            "The first letter of Dracula is M" 
        12)
            bmiTell :: (RealFloat a) => a -> String  
            bmiTell bmi  
                | bmi <= 18.5 = "You're underweight!"  
                | bmi <= 25.0 = "You're supposedly normal."  
                | bmi <= 30.0 = "You're really heavy!"  
                | otherwise   = "You're a whale, congratulations!"
        13)
            max' :: (Ord a) => a -> a -> a  
            max' a b   
                | a > b     = a  
                | otherwise = b  

            max' :: (Ord a) => a -> a -> a  
            max' a b | a > b = a | otherwise = b  
        14)
            myCompare :: (Ord a) => a -> a -> Ordering  
            a `myCompare` b  
                | a > b     = GT  
                | a == b    = EQ  
                | otherwise = LT 
        15)     
    Exercies:
        1)  Choose all right function declarations.
            func :: True -> String
            func :: True -> "Success"
            func :: Bool -> Int -> Bool
            func :: (Num a) => [a] -> a
        2)  Choose all function declarations that are took two numbers and returns whole number.
            func :: Float -> Float -> Integer
            func :: Float -> Integer -> Int
            func :: Int -> Float

        3)  Write a function that changes position of i'th and j'th element.
            change xs i j = undefined
        4)  Write a function that takes a number 1 to 12 and returns month name. Use case expressions. func 1 -> "January"
        5)  Write a function that find roots of quadratic equation. Only one is enough.
            roots x y z = undefined

-}

-- changePosition
changePosition :: Int -> Int -> [a] -> [a]
changePosition i j xs = undefined

-- month 
month :: Int -> String
month i = case i of 
        1 -> "January"
        2 -> "February"
        3 -> "March"
        4 -> ""
        5 -> ""
        6 -> ""
        7 -> ""
        8 -> ""
        9 -> ""
        10 -> ""
        11 -> ""
        12 -> ""
        _ -> "Not in scope"


-- roots
roots :: Int -> Int -> Int -> (Maybe a, Maybe b)
roots a b c = undefined
