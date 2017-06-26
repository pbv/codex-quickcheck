
module Codex.QuickCheck.Assertions (
  assert,
  (?==), (==?), (?/=), (/=?),
  (?>), (?>=), (?<), (?<=),
  (<?>)
  ) where

import Test.QuickCheck.Property

assert :: String -> Bool -> Result
assert fmt pre = if pre then succeeded else failed { reason = fmt }

infix 2 <?>
infix 4 ?==, ==?, ?/=, /=?, ?>, ?>=, ?<, ?<=

-- | Left argument should be equal to right one
(?==) :: (Eq a, Show a) => a -> a -> Result
x ?== y = assert fmt (x==y)
  where fmt = "Expected:\n\t" ++ show y ++
              "\nGot:\n\t" ++ show x ++ "\n"

(==?) :: (Eq a, Show a) => a -> a -> Result
(==?) = flip (?==)

-- | Left argument should not be equal to right one
(?/=) :: (Eq a, Show a) => a -> a -> Result
x ?/= y = assert fmt (x/=y)
  where fmt = "Expected not equal to:\n\t" ++ show y ++ "\n"

(/=?) :: (Eq a, Show a) => a -> a -> Result
(/=?) = flip (?/=)

(?>) :: (Ord a, Show a) => a -> a -> Result
x ?> y = assert fmt (x>y)
  where fmt = "Expected greater than:\n\t" ++ show y ++
              "\nGot:\n\t" ++ show x ++ "\n"

(?>=):: (Ord a, Show a) => a -> a -> Result
x ?>= y = assert fmt (x>=y)
  where fmt ="Expected greater than or equal:\n\t" ++ show y ++
             "\nGot:\n\t" ++ show x ++ "\n"
 
(?<) :: (Ord a, Show a) => a -> a -> Result
x ?< y = assert fmt (x<y)
  where fmt ="Expected less than:\n\t" ++ show y ++
             "\nGot:\n\t" ++ show x ++ "\n"


(?<=) :: (Ord a, Show a) => a -> a -> Result
x ?<= y = assert fmt (x<=y)
  where fmt ="Expected less than or equal:\n\t" ++ show y ++
             "\nGot:\n\t" ++ show x ++ "\n"


-- | append an explanatory message in case of failure
(<?>) :: Result -> String -> Property
result <?> info
  | ok result == Just False = property (result { reason = reason' })
  | otherwise               = property result
  where reason' = reason result ++ "Testing " ++ info ++ " with:"
