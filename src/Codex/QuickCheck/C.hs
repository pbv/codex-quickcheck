{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Codex.QuickCheck.C
  (
    module Foreign.C,
    module Foreign.C.Types,
    module Foreign.Ptr,
    module Foreign.Marshal.Array,
    runC,
    withCheckedArray,
    withCheckedArrayLen,
    ArrayOverflow(..),
    CArray(..),
    showArray,
    showsArray
  ) where

import Test.QuickCheck
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad (replicateM, unless)
import Control.Exception
import System.Random
import Data.List (intersperse)

-- | exceptions for array index overflows
data ArrayOverflow
  = ArrayBufferOverwrite
  | ArrayBufferUnderwrite
  deriving Show

instance Exception ArrayOverflow

-- | just a rename of unsafePerformIO (for convenience)
runC :: IO a -> a
runC = unsafePerformIO

-- | C array allocators with buffer overflow checks
withCheckedArray :: (Eq a, Random a, Storable a)
                 => [a] -> (Ptr a -> IO b) -> IO b
withCheckedArray values k = withCheckedArrayLen values (const k)

withCheckedArrayLen :: (Eq a, Random a, Storable a)
                    => [a] -> (Int -> Ptr a -> IO b) -> IO b
withCheckedArrayLen values action = do
  -- setup random sequences for canaries
  prefix <- replicateM canarySize randomIO
  postfix <- replicateM canarySize randomIO
  withArrayLen (prefix ++ values ++ postfix) $ \len ptr -> do
    let ptr' = advancePtr ptr canarySize         -- start of proper buffer
    result <- action (len - 2*canarySize) ptr'   -- run action
    -- check canaries after execution
    prefix' <- peekArray canarySize ptr
    let ptr''= advancePtr ptr (len - canarySize) -- start of trailing canary
    postfix'<- peekArray canarySize ptr''
    unless (prefix' == prefix) $ throwIO ArrayBufferUnderwrite
    unless (postfix' == postfix) $ throwIO ArrayBufferOverwrite
    -- OK, passed checks
    return result

-- | size of the canaries
canarySize :: Int
canarySize = 4


-- | utility functions

newtype CArray a
  = CArray [a] deriving (Eq, Functor, Foldable)

instance Show a => Show (CArray a) where
  showsPrec _ (CArray xs) = showsArray xs

instance Arbitrary a => Arbitrary (CArray a) where
  arbitrary = CArray <$> arbitrary
  shrink (CArray xs) = map CArray (shrink xs)


-- | show lists with C-style array literal syntax
showArray :: Show a => [a] -> String
showArray xs = showsArray xs ""

showsArray :: Show a => [a] -> ShowS
showsArray xs
  = ('{':) . (foldr (.) id $ intersperse (',':) $ map shows xs) . ('}':)


