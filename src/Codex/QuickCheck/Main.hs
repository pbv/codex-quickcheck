
module Codex.QuickCheck.Main where

import System.Exit
import Test.QuickCheck.Property
import Test.QuickCheck.Test
import Codex.QuickCheck.Args

import Control.Monad(when, void)
import Data.List (intercalate)

quickCheckMain :: Testable prop => prop -> IO ()
quickCheckMain prop = do
  args <- getQCArgs
  result <- quickCheckWithResult args prop
  when (not $ chatty args) $ putStrLn (output result)
  if isSuccess result then exitSuccess else exitFailure
