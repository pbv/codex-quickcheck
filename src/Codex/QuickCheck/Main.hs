
module Codex.QuickCheck.Main where

import System.Exit
import Test.QuickCheck.Property
import Test.QuickCheck.Test
import Codex.QuickCheck.Args

quickCheckMain :: Testable prop => prop -> IO ()
quickCheckMain prop = do
  args <- getQCArgs
  result <- quickCheckWithResult args prop
  if isSuccess result then exitSuccess else exitFailure
