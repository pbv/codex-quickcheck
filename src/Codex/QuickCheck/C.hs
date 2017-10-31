{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Codex.QuickCheck.C
  (
    module Foreign.C,
    module Foreign.C.Types,
    module Foreign.Ptr,
    module Foreign.Marshal.Array,
    withCheckedArray,
    withCheckedArrayLen,
    BufferOverflow(..),
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

import Control.Monad (replicateM, unless)
import Control.Exception
import System.Random
import Data.List (intersperse)

-- * type class instances for C types
-- | Quickcheck generator & shrinking 
-- * signed numeric types
instance Arbitrary CInt where
  arbitrary = CInt <$> arbitrary
  shrink (CInt i) = map CInt (shrink i)

instance Arbitrary CLong where
  arbitrary = CLong <$> arbitrary
  shrink (CLong i) = map CLong (shrink i)

instance Arbitrary CLLong where
  arbitrary = CLLong <$> arbitrary
  shrink (CLLong i) = map CLLong (shrink i)

instance Arbitrary CShort where
  arbitrary = CShort <$> arbitrary
  shrink (CShort i) = map CShort (shrink i)

instance Arbitrary CChar where
  arbitrary = CChar <$> arbitrary
  shrink (CChar i) = map CChar (shrink i)

-- * unsigned numeric types
instance Arbitrary CUInt where
  arbitrary = CUInt <$> arbitrary
  shrink (CUInt i) = map CUInt (shrink i)

instance Arbitrary CULong where
  arbitrary = CULong <$> arbitrary
  shrink (CULong i) = map CULong (shrink i)

instance Arbitrary CULLong where
  arbitrary = CULLong <$> arbitrary
  shrink (CULLong i) = map CULLong (shrink i)

instance Arbitrary CUChar where
  arbitrary = CUChar <$> arbitrary
  shrink (CUChar i) = map CUChar (shrink i)

-- * floating point numbers
instance Arbitrary CFloat where
  arbitrary = CFloat <$> arbitrary
  shrink (CFloat d) = map CFloat (shrink d)

instance Arbitrary CDouble where
  arbitrary = CDouble <$> arbitrary
  shrink (CDouble d) = map CDouble (shrink d)



-- | exceptions for buffer overflows
data BufferOverflow = BufferOverwrite
                    | BufferUnderwrite
                    deriving Show

instance Exception BufferOverflow


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
    unless (prefix' == prefix) $ throwIO BufferUnderwrite
    unless (postfix' == postfix) $ throwIO BufferOverwrite
    -- OK, passed checks
    return result

-- | size of the canaries
canarySize :: Int
canarySize = 4


-- * utility functions

newtype CArray a = CArray [a] deriving (Eq, Functor, Foldable)

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


