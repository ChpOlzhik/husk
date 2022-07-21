{-
Module 7. Modules
        Examples:
            1) -- Exporting modules 
            Suppose the module A exports x and y. Then this table shows what names are brought into scope by the specified import statement:

                import declaration              Names brought into scope
                import A                        x, y, A.x, A.y
                import A()                      (nothing)
                import A(x)                     x, A.x
                import qualified A              A.x, A.y
                import qualified A()            (nothing)
                import qualified A(x)           A.x
                import A hiding ()              x, y, A.x, A.y
                import A hiding (x)             y, A.y
                import qualified A hiding ()    A.x, A.y
                import qualified A hiding (x)   A.y
                import A as B                   x, y, B.x, B.y
                import A as B(x)                x, B.x
                import qualified A as B         B.x, B.y
            
            
            2)  -- Data.List
                2.1)
                    intersperse SomeElem Array(1th, 2nd, 3rd .. n^th) note. type of SomeElem eq. Array elem type.
                    result: (array[1], SomeElem, Array[2], SomeElem, Array[3] .. SomeElem, Array[n]) => a -> [a] -> [a]
                    ex. intersperse '-' "AKASHI" -> "A-K-A-S-H-I".
                2.2)
                    intercalate SomeArray Array-of-arrays => [a] -> [[a]] -> [a]
                    intercalate "-ga-" ["Akame","Kill"]
                    res. "Akame-ga-kill"
                2.3)
                    transpose [[1,1,1], [2,2,2], [3,3,3]] -> [[1,2,3], [1,2,3], [1,2,3]]
                    transpose ["aaa", "bbb", "ccc"] -> ["abc", "abc", "abc"] .
                2.4)
                    concat ["do","re","mi"] -> "doremi"
                2.5)
                    and $ map (=='.') "A.K.A_Zhuzi"
                    False
                    or $ map (=='.') "A.K.A_Zhuzi"
                    False
                2.6)
                    any (`elem` ['a'..'z']) "Hajime_No_Ippo"
                    True
                2.7)
                    take 5 $ iterate (*2) 1
                    [1, 2, 4, 8, 16]
                2.8)
                    splitAt 3 "Tibout Courtois" -> produces tuple of array divinding it at to, first elem size at most 3
                    ("Tib","out Courtois")
                2.9)
                    takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
                    [6,5,4]
                    
                    takeWhile (/=' ') "This is a sentence"
                    "This"
                2.10)
                    span
                2.11)
                    break
                2.12)
                    sort
                2.13)
                    group
                2.14)
                    inits
                    tails
                2.15)
                    isInfixOf
                    isPrefixOf
                    isSuffixOf
                2.16)
                    partition
                2.17)
                    find
                2.18)
                    elemIndex
                2.19)
                    findIndex
                2.20)
                    zipWith3
                2.21)
                    lines
                    unlines
                2.22)
                    nub
                2.23)
                    insert
                    delete
                2.24)
                    intersect
                    union
                2.25)
                    nubBy, deleteBy, unionBy, intersectBy and groupBy.
            3) -- Data.Char

                3.1)
                    isControl checks whether a character is a control character.
                3.2)
                    isSpace checks whether a character is a white-space characters. That in- cludes spaces, tab characters, newlines, etc.
                3.3)
                    isLower checks whether a character is lower-cased.
                3.4)
                    isUpper checks whether a character is upper-cased.
                3.5)    
                    isAlpha checks whether a character is a letter.
                3.6)
                    isAlphaNum checks whether a character is a letter or a number.
                3.7)
                    isPrint checks whether a character is printable. Control characters, for instance, are not printable. isDigit checks whether a character is a digit.
                3.8)
                    isOctDigit checks whether a character is an octal digit.
                3.9)
                    isHexDigit checks whether a character is a hex digit.
                3.10)
                    isLetter checks whether a character is a letter.
                3.11)    
                    isMark checks for Unicode mark characters. Those are characters that combine with preceding letters to form latters with accents. Use this if you are French.
                3.12)    
                    isNumber checks whether a character is numeric.
                3.13)    
                    isPunctuation checks whether a character is punctuation.
                3.14)    
                    isSymbol checks whether a character is a fancy mathematical or currency symbol.
                3.15)    
                    isSeparator checks for Unicode spaces and separators.
                3.16)    
                    isAscii checks whether a character falls into the first 128 characters of the Unicode character set. isLatin1 checks whether a character falls into the first 256 characters of Unicode.
                3.17)    
                    isAsciiUpper checks whether a character is ASCII and upper-case.
                3.18)    
                    isAsciiLower checks whether a character is ASCII and lower-case.
            4) -- Data.Map
                4.1) Map.map
                4.2) Map.fromList
                4.3) Map.empty Map.null
                4.4) Map.insert
                4.5) Map.findKey
                4.7) Map.singleton
                4.8) Map.fromListWith 
            5) -- Data.Set
                5.1) Set.fromList
                5.2) Set.intersection
                5.3) Set.difference
                5.4) Set.union
                5.5) Set.null Set.empty
                5.6) Set.size
                5.7) Set.singleton
                5.8) Set.filter
            6) -- Own modules
                module Geometry
                  ( sphereVolume
                  , sphereArea
                  , cubeVolume
                  , cubeArea
                  , cuboidArea
                  , cuboidVolume
                  ) where
                  sphereVolume :: Float -> Float
                  sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)
                  sphereArea :: Float -> Float
                  sphereArea radius = 4 * pi * (radius ^ 2)
                  cubeVolume :: Float -> Float
                  cubeVolume side = cuboidVolume side side side
                  cubeArea :: Float -> Float
                  cubeArea side = cuboidArea side side side
                  cuboidVolume :: Float -> Float -> Float -> Float
                  cuboidVolume a b c = rectangleArea a b * c
                  cuboidArea :: Float -> Float -> Float -> Float
                  cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 +
                  rectangleArea c b * 2
                  rectangleArea :: Float -> Float -> Float
                  rectangleArea a b = a * b
-}
--        Exercises:
--        1) Create module "Pifagor" with following:

thirdSide :: Float -> Float -> Float -> Float
thirdSide side1 side2 angle = undefined

area :: Float -> Float -> Float -> Float
area side1 side2 angle = undefined