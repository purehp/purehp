module Functions where

import Functions.Inner (fun3)
import Functions.Inner as FI

fun2 :: Int -> Int -> Int
fun2 a b = fun3 a b

foo :: Int
foo = fun2 1 2

bar :: Int
bar = fun3 5 5
