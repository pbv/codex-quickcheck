{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveFunctor,
             TypeSynonymInstances,
             FlexibleInstances   #-}
module Codex.QuickCheck.C.Types
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
    toArray,
    fromArray,
    showArray,
    showsArray,
    fromBool,
    toBool,
    CType(..)
  ) where

import Test.QuickCheck
import qualified Codex.QuickCheck.Modifiers as M
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

toArray :: [a] -> CArray a
toArray = CArray

fromArray :: CArray a -> [a]
fromArray (CArray xs) = xs

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

--type Name = String

-- | show C type name for a given variable
class CType t where
  cbase :: t -> ShowS
  cmodifiers :: t -> ShowS

instance CType CInt where
  cbase v = ("int "++)
  cmodifiers v = id

instance CType Int where
  cbase v = ("int "++)
  cmodifiers v = id

instance CType (M.NonNegative CInt) where
  cbase v = ("int " ++)
  cmodifiers v = id

instance CType (M.Positive CInt) where
  cbase v = ("int " ++)
  cmodifiers v = id

instance CType CDouble where
  cbase v = ("double " ++)
  cmodifiers v = id

instance CType CUInt where
  cbase v = ("unsigned " ++)
  cmodifiers v = id

instance CType Char where
  cbase v = ("char "++)
  cmodifiers v = id

instance CType CChar where
  cbase v = ("char "++)
  cmodifiers v = id

instance CType String where
  cbase v = ("char *"++)
  cmodifiers v = id

instance CType a => CType (CArray a) where
  cbase (CArray vs)
    = cbase (head vs)
  cmodifiers (CArray vs)
    = ("["++) . shows n . ("]"++) . cmodifiers (head vs)
    where n = length vs

