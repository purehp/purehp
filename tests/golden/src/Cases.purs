module Cases where

foo :: Int -> Int
foo x = case x of
  1 -> 0
  n -> n
