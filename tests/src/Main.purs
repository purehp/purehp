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

bar1 :: Bar
bar1 = A

bar2 :: Bar
bar2 = B 1 2 3

bar3 :: Int -> Bar
bar3 = B 1 2

if1 :: Int -> Int
if1 x = if true then x else 1
