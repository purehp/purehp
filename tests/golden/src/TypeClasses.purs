module TypeClasses where

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

data Foo a = Foo a

instance functorFoo :: Functor Foo where
  map f (Foo i) = Foo (f i)

bar :: Foo Int -> Foo Int
bar = map (\_ -> 2)

baz :: Foo Int
baz = bar (Foo 1)
