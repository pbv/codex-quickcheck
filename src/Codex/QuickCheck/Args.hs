
module Codex.QuickCheck.Args (getQCArgs) where

import           Data.List (foldl')
import           System.Environment
import           System.Console.GetOpt
import           Test.QuickCheck (Args(..), stdArgs)
import qualified Test.QuickCheck.Random as QC

options :: [OptDescr (Args -> Args)]
options =
  [ Option [] ["maxSuccess"]
    (ReqArg (\str args -> args {maxSuccess = read str}) "INT")
    "Number of tests to pass"
  , Option [] ["maxSize"]
    (ReqArg (\str args -> args {maxSize = read str}) "INT")
    "Maximum test data size to generate"
  , Option [] ["maxDiscardRatio"]
    (ReqArg (\str args -> args{maxDiscardRatio=read str}) "INT")
    "Maximum ratio of discarded test cases"
  , Option [] ["randSeed"]
    (ReqArg (\str args -> let seed = read str
                          in args {replay = Just (QC.mkQCGen seed,0)}) "INT")
    "Fix seed for random test case generator"
  , Option ['q'] ["quiet"]
    (NoArg (\args -> args {chatty = False}))
    "Reduce verbosity"
  ]


getQCArgs :: IO Args
getQCArgs = do
  argv <- getArgs
  arg0 <- getProgName
  case getOpt Permute options argv of
    (o, _, []) -> return (foldl' (flip id) stdArgs o)
    (o, _, errs) ->
      let header = "usage: " ++ arg0 ++ " [OPTION...]"
      in ioError (userError (concat errs ++ usageInfo header options))
  

