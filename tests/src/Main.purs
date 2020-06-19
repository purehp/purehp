module Main where

-- import Prelude

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

fun1 :: Int -> Int
fun1 arg = arg

rec1 :: { x :: Int, y :: String, z :: Int -> Int }
rec1 = { x: 1, y: "Hello", z: \x -> x }

newtype Foo = Foo Int

data Bar = A | B Int Int Int

-- b1 = B 1 2 3
