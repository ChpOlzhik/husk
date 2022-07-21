{-
    Examples:
            -- 3.1
            -- :t for type check

            {-
                :t 1
                :t 1.2
                :t 'с'
                :t "gl&hf"
                -- also expressions
                :t True || False
                :t head "gl&hf"
            -}

            -- Functions also have types
            removeNonUppercase :: String -> String
            removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

            {-
                To see types after any expression
                :set +t
                :t uppercase
                :t not
                uppercase "wassUP"
                5 == 6
            -}


            -- 3.2
            {-
                :t head
                :t fst
            -}

            -- 3.3 Typeclasses
            {-
                Eq
                    :t (==) or :t (!=) 
                Ord
                    :t (>) or :t compare
                Show
                    :t show
                Read
                    :t read
                Enum
                    :t [1..5]
                    :t succ
                Bounded
                    :t minBound or :t maxBound
                Num
                    :t 20
                    20 :: Int
                    20 :: Integer
                    20 :: Float
                    20 :: Double
                Integral
                    Int and Integer
                Floating
                    Float and Double

        Exercies:
            1) What is the type of following expression? 
                1.1) ’h’:’e’:’l’:’l’:’o’:[]
                1.2) [5,’a’]
                1.3) (5,’a’)
                1.4) (5::Int) + 10
                1.5) (5::Int) + (10::Double)
            2) What is the type of following expression?
                2.1) snd
                2.2) head
                2.3) null
            3) Try to guees type of function below. 
                middle (_,a,_) = a
            4) Type class that support equality testing ? Eq
            5) Type class that support comparing ? Ord
            6) Type class that converts any type to string ? Show
-}
-}

-- Define type of function
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r