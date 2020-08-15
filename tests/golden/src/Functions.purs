module Functions where

import Functions.Inner (fun3)
-- import Functions.Inner as FI

-- val1 :: Int
-- val1 = 3

-- fun1 :: Int -> Int
-- fun1 arg = arg

fun2 :: Int -> Int -> Int
fun2 a b = fun3 a b

-- baz :: Int
-- baz = foo

foo :: Int
foo = fun2 1 2

-- baz2 :: Int
-- baz2 = bar

bar :: Int
bar = fun3 5 5

-- quux :: Int
-- quux = FI.fun3 10
