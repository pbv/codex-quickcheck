
module Codex.QuickCheck.Runner where

import System.Exit
import Test.QuickCheck.Property
import Test.QuickCheck.Test

import Codex.QuickCheck.Args

quickCheckRunner :: Testable prop => prop -> IO ()
quickCheckRunner prop = do
  args <- getQCArgs
  result <- quickCheckWithResult args prop
  if isSuccess result then exitSuccess else exitFailure
