{-
  Various helper operators for assertions
-}
module Codex.QuickCheck.Assertions (
  testing,
  (?==), (==?), (?/=), (/=?),
  (?>), (?>=), (?<), (?<=),
  (<?>),
  absoluteEq,
  relativeEq,
  assertCond,
  assertAbsEq,
  assertRelEq
  ) where

import Test.QuickCheck.Property

infix 2 <?>
infix 4 ?==, ==?, ?/=, /=?, ?>, ?>=, ?<, ?<=

-- | Left argument should be equal to right one
(?==) :: (Eq a, Show a) => a -> a -> Property
x ?== y = counterexample fmt (x==y)
  where fmt = "Expected:\n\t" ++ show y ++
              "\nGot:\n\t" ++ show x ++ "\n"

(==?) :: (Eq a, Show a) => a -> a -> Property
(==?) = flip (?==)

-- | Left argument should not be equal to right one
(?/=) :: (Eq a, Show a) => a -> a -> Property
x ?/= y = counterexample fmt (x/=y)
  where fmt = "Expected not equal to:\n\t" ++ show y ++ "\n"

(/=?) :: (Eq a, Show a) => a -> a -> Property
(/=?) = flip (?/=)

(?>) :: (Ord a, Show a) => a -> a -> Property
x ?> y = counterexample fmt (x>y)
  where fmt = "Expected greater than:\n\t" ++ show y ++
              "\nGot:\n\t" ++ show x ++ "\n"

(?>=):: (Ord a, Show a) => a -> a -> Property
x ?>= y = counterexample fmt (x>=y)
  where fmt ="Expected greater than or equal:\n\t" ++ show y ++
             "\nGot:\n\t" ++ show x ++ "\n"
 
(?<) :: (Ord a, Show a) => a -> a -> Property
x ?< y = counterexample fmt (x<y)
  where fmt ="Expected less than:\n\t" ++ show y ++
             "\nGot:\n\t" ++ show x ++ "\n"


(?<=) :: (Ord a, Show a) => a -> a -> Property
x ?<= y = counterexample fmt (x<=y)
  where fmt ="Expected less than or equal:\n\t" ++ show y ++
             "\nGot:\n\t" ++ show x ++ "\n"


-- | append explanatory messages in case of failure
(<?>) :: Property -> String -> Property
prop <?> info
  = counterexample ("Testing " ++ info) prop

testing :: String -> Property -> Property
testing info prop
  = counterexample ("Testing " ++ info ++ " with:") prop

-- | comparing floating point numbers
absoluteEq :: RealFloat a => a -> a -> a -> Bool
absoluteEq eps x y = abs (x-y) <= eps

relativeEq :: RealFloat a => a -> a -> a -> Bool
relativeEq eps x y = abs (x-y) <= max (abs x) (abs y) * eps

-- blames the second argument when a condition fails
assertCond :: Show a => (a -> a -> Bool) -> a -> a -> Property
assertCond cond answer trial = counterexample msg (cond answer trial)
  where msg = "Expected:\n\t" ++ show answer ++
              "\nGot:\n\t" ++ show trial ++ "\n"

assertAbsEq eps = assertCond (absoluteEq eps)

assertRelEq eps = assertCond (relativeEq eps)


