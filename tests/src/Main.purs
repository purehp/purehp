module Main where

x :: Int
x = 5

y :: Int
y =  3

foo :: String
foo = "Hello World!"

bar :: Char
bar = 'a'

z :: Array Int
z = [1, 2, 3, 4]

obj :: { x :: Int, y :: String }
obj = { x: 1, y: "Hello" }

func :: Int -> Int
func arg = arg

-- foo :: Int -> Int -> Int
-- foo a b = a + b
