{-# OPTIONS_GHC -fwarn-tabs #-}
{-
Name: Joseph Brady
Notes: 

problem 1 took 2 hours problem 2 took 3 hours
-}

module HW05 where

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW05.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- mod returns remainder
lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit x = x `mod` 10

-- number is minused by the last digit leaving a 0 place which is
-- which is removed by div 10
dropLastDigit :: Integer -> Integer
dropLastDigit 0 = 0
dropLastDigit x = (x - ( lastDigit x ) ) `div` 10

-- appends the last digit to the back of the list
-- recursively calls todigits while shortening the list with droplast
-- if the number is less than 0 then returns an empty list
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x < 0 = []
  | otherwise = toDigits ( dropLastDigit x ) ++ [ lastDigit x]




--this goes through the list and for the second element it is doubled
--it doubles the second element and cars it with :

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper (x:y:xs) = x:(2 * y):(doubleEveryOtherHelper xs)
doubleEveryOtherHelper xs = xs


--so the right ones are doubled needs reverse then reversed to be in order
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherHelper . reverse


--sums the digits with the sum function for number that is changed into a list
-- of elements recursively calls sumDigits while shorten the list xs like cdr
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum . toDigits) x + sumDigits xs


--changes the number x to its digits then doubles it
--sums those digits
--then checks if 0 by moding by 10
validate :: Integer -> Bool
validate x = (mod (sumDigits (doubleEveryOther (toDigits x ) ) ) 10) == 0

--base case is an empty list
--if only 1 then just return the move a to b 
--If you have an odd number of disks, move it clockwise 
-- move it in the order src temp dst
-- otherwise move it anticlockwise.

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src dst _ = [(src, dst)]
hanoi n src dst tmp = (hanoi (n-1) src tmp dst) 
		++ (hanoi 1 src dst tmp)
		++ (hanoi (n-1) tmp dst src)
