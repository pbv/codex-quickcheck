{-
   Properties for testing C code;
   generates C declarations for generating test code (e.g. for debuggers)
 -}

module Codex.QuickCheck.C.Property where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property hiding (forAll, forAllShrink)
import Codex.QuickCheck.C.Types (CType(..))

forArbitrary ::
  (CType a, Show a, Arbitrary a, Testable prop)
  => String
  -> (a -> prop)
  -> Property
forArbitrary name pf
  = forAllShrink name arbitrary shrink pf

forAllShrink ::
  (CType a, Show a, Testable prop)
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
                   else cdecl x label ""

forAll ::
  (CType a, Show a, Testable prop)
  => String
  -> Gen a
  -> (a -> prop)
  -> Property
forAll label gen pf
  = forAllShrink label gen (const []) pf

letArg ::
  (CType a, Show a, Testable prop)
  => String
  -> a
  -> (a -> prop)
  -> Property
letArg name val
  = forAll name (return val)


cdecl :: (CType t, Show t) => t -> String -> ShowS
cdecl v name
  = cbase v
  . (name ++)
  . cmodifiers v
  . (" = "++)
  . shows v
  . (";"++)
