{-
   Top-level module for testing Haskell code; this just re-exports various sub-modules
-}

module Codex.QuickCheck
   ( module Test.QuickCheck.Gen
   , module Test.QuickCheck.Arbitrary
   , module Test.QuickCheck.Property
   , module Test.QuickCheck.Exception
   , module Test.QuickCheck.Random
   , module Test.QuickCheck.Test
   , module Test.QuickCheck.All
   , module Codex.QuickCheck.Modifiers
   , module Codex.QuickCheck.Assertions
   , module Codex.QuickCheck.Property
   , module Codex.QuickCheck.Main
   ) where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property hiding (forAll, forAllShrink, Result(..))
import Test.QuickCheck.Exception
import Test.QuickCheck.Random
import Test.QuickCheck.Test 
import Test.QuickCheck.All

import Codex.QuickCheck.Modifiers
import Codex.QuickCheck.Property
import Codex.QuickCheck.Assertions
import Codex.QuickCheck.Main

