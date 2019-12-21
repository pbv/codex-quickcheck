{-
  Test a C function to check if a string is a strong password:
  * at least length 6
  * at least one lowercase, one uppercase and one digit character
-}
module Main where

import Codex.QuickCheck
import Control.Monad
import Control.Exception
import Data.Char

-- | import the C function for testing
-- result is a logic value (0/1)
foreign import ccall "strong_passwd" strong_passwd :: CString -> IO CInt

-- | functional wrapper over the C code
strong_passwd_wrapper :: String -> CInt
strong_passwd_wrapper str
  = runC $
    withCString str $ \ptr -> do
       result <- strong_passwd ptr
       str' <- peekCAString ptr
       unless (str == str') $
         throwIO $ userError "function modified argument string"
       return result

-- | reference solution in Haskell
strong_spec :: String -> Bool
strong_spec xs
  = length xs >= 6 && any isUpper xs && any isLower xs && any isDigit xs

-- | correctness property
prop_correct :: Property
prop_correct
  = testing "strong_passwd" $
    forAllShrink "str" genPasswd shrinkPasswd $
    \xs -> strong_passwd_wrapper xs ?== fromBool (strong_spec xs)

-- | generate password using charateres com '0' to 'z'
genPasswd = listOf (choose ('0', 'z'))

-- | shrink passwords, preserving the condition above 
shrinkPasswd = shrinkMap (filter (\c -> c>='0' && c<='z')) id

-- | main entry point
-- parse commmand line args and run tests
main = quickCheckMain prop_correct
