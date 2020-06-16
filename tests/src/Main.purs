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

data Bar = A Int | B String | C

-- data Foo = Foo Int

-- data Unit = Unit


-- _string :: String
-- _string = "Hello World!"

-- _char :: Char
-- _char = 'a'

-- _array :: Array Int
-- _array = [1, 2, 3, 4]

-- _record :: { x :: Int, y :: String }
-- _record = { x: 1, y: "Hello" }

-- _fun :: Int -> Int
-- _fun arg = arg + arg


-- func :: Int -> Int
-- func arg = arg + 2

-- func3 :: Int -> Int -> Int
-- func3 arg arg2 = arg + arg2
