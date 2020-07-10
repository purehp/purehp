module DataConstructors where

newtype NewFoo = NewFoo Int

data Bar = A | B Int Int Int

bar1 :: Bar
bar1 = A

bar2 :: Bar
bar2 = B 1 2 3

bar3 :: Int -> Bar
bar3 = B 1 2
