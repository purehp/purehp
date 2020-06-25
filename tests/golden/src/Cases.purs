module Cases where

foo :: Int -> Int
foo x = case x of
  1 -> 0
  2 -> 1
  _ -> 2

bar :: Boolean -> String
bar x = if x then "True!" else "False!"
