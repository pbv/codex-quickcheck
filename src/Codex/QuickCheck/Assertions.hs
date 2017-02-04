
module Codex.QuickCheck.Assertions (
  (?==), (==?), (<?>)
  ) where

import Test.QuickCheck.Property

assert :: String -> Bool -> Result
assert fmt pre = if pre then succeeded
                   else failed { reason = fmt }

infix 2 <?>
infix 4 ?==, ==?

  
-- | Left argument should be equal to right one
(?==) :: (Eq a, Show a) => a -> a -> Result
x ?== y = assert fmt (x==y)
  where fmt = "Expected:\n\t" ++ show y ++
              "\nGot:\n\t" ++ show x ++ "\n"

(==?) :: (Eq a, Show a) => a -> a -> Result
(==?) = flip (?==)


-- | append an explanatory message in case of failure
(<?>) :: Result -> String -> Result
result <?> msg    
  | ok result == Just False = result { reason = reason result ++ msg }
  | otherwise               = result 
