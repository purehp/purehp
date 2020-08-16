module Literals where

int1 :: Int
int1 = 5

int2 :: Int
int2 = int1

string1 :: String
string1 = "Hello World!"

char1 :: Char
char1 = 'C'

array1 :: Array Int
array1 = [1, 2, 3, 4]

rec1 :: { x :: Int, y :: String, z :: Int -> Int }
rec1 = { x: 1, y: "Hello", z: \x -> int1 }
