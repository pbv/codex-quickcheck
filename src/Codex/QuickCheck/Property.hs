
module Codex.QuickCheck.Property where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property hiding (forAll, forAllShrink)


forArbitrary ::
  (Show a, Arbitrary a, Testable prop)
  => String
  -> (a -> prop)
  -> Property
forArbitrary name pf
  = forAllShrink name arbitrary shrink pf 

forAllShrink :: 
  (Show a, Testable prop) 
  => String
  -> Gen a
  -> (a -> [a])
  -> (a -> prop)
  -> Property
forAllShrink label gen shrinker pf =
  again $
  MkProperty $
  gen >>= \x' ->
    unProperty $
    shrinking shrinker x' $ \x ->
      counterexample (showf x) (pf x)
  where
    showf x = '\t':if null label then show x
                   else label ++ " = " ++ show x

forAll ::
  (Show a, Testable prop)
  => String 
  -> Gen a
  -> (a -> prop)
  -> Property
forAll label gen pf
  = forAllShrink label gen (const []) pf

letArg ::
  (Show a, Testable prop)
  => String
  -> a
  -> (a -> prop)
  -> Property
letArg name val
  = forAll name (return val)

