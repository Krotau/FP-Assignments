-- Authors: Bram van den Berg (1062047) and Tim Smeets (1065376)

--------------------------------------------------------------------------------------
-- Exercise 2.3

module Char where
import Data.Char

-- equal      :: String -> String -> Bool
-- isNumeral  :: String -> Bool
-- isBlank    :: String -> Bool
-- fromDigit  :: Char -> Int
-- toDigit    :: Int -> Char
-- shift      :: Int -> Char -> Char

msg  ::  String
msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
        \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

-- This function compares two string by letter, not case sesitive.
equal :: String -> String -> Bool
equal x y = (map toLower x) == (map toLower y)

-- Question: creates tests whether a string consists solely of digits or white space.

-- These functions below determine if a string is completely empty
isSingleBlank :: Char -> Bool
isSingleBlank chr = (chr == ' ')

isBlank :: String -> Bool
isBlank str = (and (map isSingleBlank str))

-- These functions below determine if a string is composed out of purely digits.
isNumeral :: String -> Bool
isNumeral str = (and (map isDigit str))

-- below functions convert chars to digits
fromDigit :: Char -> Int
fromDigit chr = if isNumeral [chr] then read [chr] else (-1) -- Foutmelding?

toDigit :: Int -> Char
toDigit digit = head (show digit)

lowerBound = 65
upperBound = 90

-- Ceasar cipher
shift :: Int -> Char -> Char
shift offset ' ' = ' '
shift offset character = let
        var1 = Data.Char.ord(character) + offset -- get shifted ASCII code of the character
        var2 = if var1 > upperBound then lowerBound + (var1 - upperBound - 1) else var1 -- -1 because of the the lowerbound is slightly off
        in Data.Char.chr(var2) -- return the new letter that is shifted

defaultOffset = 19

shiftWithDefinedOffset :: Char -> Char
shiftWithDefinedOffset character = shift defaultOffset character

-- usage -> shiftMessage msg
shiftMessage :: String -> String
shiftMessage message = map shiftWithDefinedOffset message

-- Answer: "FABER EST SUAE QUISQUE FORTUNAE APPIUS CLAUDIUS CAECUS DICTUM ARCNUM EST NEUTRON"

--------------------------------------------------------------------------------------
-- Exercise 2.4

-- Question: Explore the difference between machine-integers of type Int and mathematical integers of type Integer. 
--           The type annotation :: Int instructs the compiler to perform the multiplications using machine-integers.
--           Repeat the exercise using the type annotation :: Integer. What do you observe? Can you explain the differences?

-- Answer:
-- When using the Int data type the factorial of 21 is negative and when reaching the factorial of 66 the outcome is 0.
-- This is because the result of factorial 66 is outside of the range of 32 and 64-bit integers. 
-- When using the Integer data type it can make large integers by using storage available on the machine.
-- So it can show the full integer of a calculation such as the factorial of 66.