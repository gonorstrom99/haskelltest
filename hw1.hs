--homework 1, part 1
toDigits :: Integer -> [Integer]
toDigits x = if x<0 then [] else if abs x<10 then [x]
             else toDigits (div x 10) ++ [mod x 10]
toDigitsRev :: Integer -> [Integer]
toDigitsRev x =if x<0 then [] else if abs x<10 then [x]
             else [mod x 10] ++ toDigitsRev (div x 10)
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = if length (x) == 2 then [(head x) * 2, (last x)] else if length (x) ==1 then x
                     else doubleEveryOther (    
                        fst (
                            splitAt 
                                (length x -2) x
                            
                        ))
                    ++ 
                     doubleEveryOther (snd (splitAt (length x -2) x))
sumOfNum :: Integer -> Integer  
sumOfNum x = if x < 10 then x
            else mod x 10 + sumOfNum (div x 10)
sumDigits :: [Integer] -> Integer
sumDigits x = if length x == 1 then sumOfNum (x !! 0)
              else sumOfNum (x !! 0) + sumDigits (tail x)
validate :: Integer -> Bool
validate x = if mod (sumDigits (doubleEveryOther (toDigits x))) 10 ==0 then True
            else False
--homework 1, part 2
--tower of hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x p1 p2 p3 = [("a","b")]