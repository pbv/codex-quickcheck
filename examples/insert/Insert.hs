--
-- Insert a value into an array preserving order
--
module Main where

import           Codex.QuickCheck
import qualified Data.List as List
import           Data.Char

foreign import ccall "insert_value"
   insert_value :: Ptr CInt -> CInt -> CInt -> IO ()

insert_wrapper :: CArray CInt -> CInt -> CArray CInt
insert_wrapper vec val =
  runC $
  withCheckedArrayLen (fromArray vec ++ [0]) $ \size ptr -> do
    insert_value ptr (fromIntegral size - 1) val
    toArray <$> peekArray size ptr
    
prop_insert_correct :: Property
prop_insert_correct 
  = testing "insert_value" $
    forAllShrink "vec" ordArray ordShrink $ \vec ->
    forArbitrary "val" $ \val ->
       insert_wrapper vec val ?== toArray (List.insert val $ fromArray vec)

ordArray = CArray <$> orderedList
ordShrink = shrinkMap (toArray . List.sort) fromArray

main = quickCheckMain prop_insert_correct

               
