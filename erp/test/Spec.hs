{-#LANGUAGE GADTs #-}
main :: IO ()
main = putStrLn "Test suite not yet implemented"

class Foo a where
  foo :: a -> String

class Bar a where
  bar :: a -> String

data Baz f b where
  Cons :: (Foo f, Bar b) => f -> b -> Baz f b

testMethod :: Baz f z -> String
testMethod (Cons f z) = (foo f) ++ (bar z)
