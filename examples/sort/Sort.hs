--
-- Sort an array in decreasing order
--
module Main where

import           Codex.QuickCheck.C
import qualified Data.List as List
import           Data.Char

foreign import ccall "sort_desc" sort_desc :: Ptr CInt -> CInt -> IO ()

sort_wrapper :: CArray CInt -> CArray CInt
sort_wrapper (CArray values) =
  runC $
  withCheckedArrayLen values $ \size ptr -> do
     sort_desc ptr (fromIntegral size) 
     values'<- peekArray size ptr
     return (CArray values')
     
prop_correct :: Property
prop_correct 
  = testing "sort_desc(vec, n)" $
    forArbitrary "vec" $ \vec ->
       letArg "n" (toCInt $ length vec) $ \_ ->
       sort_wrapper vec ?== toArray (reverse $ List.sort $ fromArray vec)


toCInt :: Int -> CInt
toCInt = fromIntegral

main = quickCheckMain prop_correct

               
