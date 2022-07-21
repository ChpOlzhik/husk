{-
Module 8. Making Our Own Types and Type - classes
        Examples:
            1) Algebraic data types intro

            data Shape = Circle Float Float Float | Rectangle Float Float Float Float

            surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
            2) 
                data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)  
            3) 
                data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show) 
            4) 
                tellCar :: Car -> String  
                tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  

                ghci> let stang = Car {company="Ford", model="Mustang", year=1967}  
                ghci> tellCar stang  
                "This Ford Mustang was made in 1967" 
            5) 
                Type synonyms

                type String = [Char]  


                phoneBook :: [(String,String)]  
                phoneBook =      
                    [("betty","555-2938")     
                    ,("bonnie","452-2928")     
                    ,("patsy","493-2928")     
                    ,("lucille","205-2928")     
                    ,("wendy","939-8282")     
                    ,("penny","853-2492")     
                    ] 

            6) data List a = Empty | Value a (List a) deriving (Show, Read, Eq, Ord) 


-}
--        Exercises:
--            1) binary tree, construct binary tree from array
data Tree a = Leaf | Node Integer (Tree a) (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a

--            2) 
