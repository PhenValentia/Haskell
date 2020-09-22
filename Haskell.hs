-- Example module
-- Gabriele Keller, August 2015
--
-- This is a simple example of a module definition

module Test
where

import Numeric 
import Data.Char 

-- calculates the arithmetic mean of two numbers
--
arithmetic_mean :: Fractional a => a -> a -> a
arithmetic_mean x y  = (x + y)  / 2

-- calculates the harmonic mean of two numbers
--
harmonic_mean :: Fractional a => a -> a -> a
harmonic_mean x y  = 2 * x * y / (x + y)

natSum :: Int -> Int
natSum 0 = 0
natSum x = x + natSum (x-1)






last_letters :: String -> [Char]
last_letters "" = ""
last_letters str = last str : [last (init "str")]









fourthBit :: Int -> Char
fourthBit i =  binaryDigitAt (reverse(showIntAtBase 2 intToDigit i "")) 4

binaryDigitAt:: [Char] -> Int -> Char
binaryDigitAt [] n = '0'
binaryDigitAt [x] n = '0'
binaryDigitAt (x:xs) n
    | n == 1    = x
    | otherwise = binaryDigitAt xs (n-1)







countNonUnique :: [Int] -> Int
countNonUnique [] = 0
countNonUnique [x] = 0
countNonUnique (x:xs) = countingNonUnique (x:xs) (x:xs) []

countingNonUnique :: [Int] -> [Int] -> [Int] -> Int
countingNonUnique [] (x:xs) (z:zs) = 0
countingNonUnique [y] (x:xs) z 
    | not(elem y z)    = checkInstance (x:xs) y 0
    | otherwise        = 0
countingNonUnique (y:ys) (x:xs) z
    | not(elem y z)    = checkInstance (x:xs) y 0 + countingNonUnique ys (x:xs) (z ++ [y])
    | otherwise             = countingNonUnique ys (x:xs) z

checkInstance :: [Int] -> Int -> Int -> Int
checkInstance [] n c      = 0
checkInstance [x] n c 
    | c == 1 && x == n    = 1
    | otherwise           = 0
checkInstance (x:xs) n c 
    | c == 1 && x == n    = 1
    | c == 0 && x == n    = checkInstance xs n 1 
    | otherwise           = checkInstance xs n c 





checkPar :: String -> Int
checkPar "" = 0
checkPar [x]
    | x == '(' || x == ')'  = 1
    | otherwise             = 0
checkPar (x:xs) = countPar 0 0 (x:xs)

countPar :: Int -> Int -> [Char] -> Int
countPar l r [] = 0
countPar l r [x]
    | x == '('              = r+1
    | x == ')' && l >= 1    = r-1
    | x == ')' && l == 0    = r+1
    | otherwise             = r+0
countPar l r (x:xs)
    | x == '('              = countPar (l+1) (r+1) xs
    | x == ')' && l >= 1    = countPar (l-1) (r-1) xs 
    | x == ')' && l == 0    = countPar l (r+1) xs
    | otherwise             = countPar l r xs
	
	
findMatchingPar :: String -> Int
findMatchingPar "" = 0
findMatchingPar [x] = 0
findMatchingPar (x:xs) = countUntil x xs

countUntil :: Int -> String -> Int
countUntil i "" = 0
countUntil i [x] = 0
countUntil i (x:xs)
	| i == 0	= calcIndex 0 0 xs
	| otherwise 	= countUntil i-1 xs

calcIndex :: Int -> Int -> String -> Int
calcIndex i j "" = 0
calcIndex i j [x] = 
| j == 0 && x == “)” 	= i
| otherwise 		= 0
calcIndex i j (x:xs) = 
| j == 0 && x == “)” 	= i
	| x == “)” 		= calcIndex i+1 j-1 xs
| x == “(” 		= calcIndex i+1 j+1 xs
| otherwise 		= 0


