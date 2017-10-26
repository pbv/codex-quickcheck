{-# LANGUAGE RecordWildCards #-}

module Codex.QuickCheck.Args where

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Random as QC

-- | wrapper type for serializing Quickcheck arguments to/from strings
data CodexArgs = CodexArgs { maxSuccess      :: Int
                           , maxSize         :: Int
                           , maxDiscardRatio :: Int
                           , randSeed        :: Maybe Int
                           , chatty          :: Bool
                           } deriving (Eq, Show, Read)


defaultArgs :: CodexArgs
defaultArgs = CodexArgs { maxSuccess = 100
                        , maxSize   = 100
                        , maxDiscardRatio = 10
                        , randSeed = Nothing
                        , chatty = True
                        }

makeQCArgs :: CodexArgs -> QC.Args
makeQCArgs CodexArgs{..}
  = QC.Args { QC.maxSuccess = maxSuccess
            , QC.maxSize   = maxSize
            , QC.maxDiscardRatio = maxDiscardRatio
            , QC.replay = fmap (\s -> (QC.mkQCGen s, 0)) randSeed
            , QC.chatty = chatty
            }

