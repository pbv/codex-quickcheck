{-
  Test a C implementation of bounded queues
  Based on "state-machine" approach presented in the paper:
  "Testing the Hard Stuff and Staying Sane" by J. Hughes
-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (delete)
import Control.Monad.State
import Foreign hiding (new)
import Foreign.C.Types
import Foreign.Ptr

import Test.QuickCheck
import Test.QuickCheck.Monadic

data QueueStruct  -- abstract
type Queue = Ptr QueueStruct


foreign import ccall "new" new :: CSize -> IO Queue
foreign import ccall "delete" delete :: Queue -> IO ()
foreign import ccall "is_empty" is_empty :: Queue -> IO CInt
foreign import ccall "is_full" is_full :: Queue -> IO CInt
foreign import ccall "enqueue" enqueue :: Queue -> CInt -> IO CInt
foreign import ccall "dequeue" dequeue :: Queue -> IO CInt

-- | queue commands 
data Command
  = Enqueue CInt 
  | Dequeue 
  | IsEmpty 
  | IsFull
  deriving (Eq, Show)

-- | a response maybe be nothing or an int
type Response = Maybe CInt

-- | generate and shrink arbitrary commands
instance Arbitrary Command where
  arbitrary = frequency [ (2, Enqueue <$> arbitrary)
                        , (2, return Dequeue)
                        , (1, return IsEmpty)
                        , (1, return IsFull)
                        ]
  shrink (Enqueue n) = Enqueue <$> shrink n
  shrink _           = []


-- | execute a command using the implementation
execute :: Queue -> Command -> IO Response
execute q (Enqueue v) = enqueue q v >> return Nothing
execute q Dequeue = Just <$> dequeue q
execute q IsEmpty = Just <$> is_empty q
execute q IsFull  = Just <$> is_full q

-- | purely functional model for bounded queues 
data Model 
  = Model { capacity :: Int
          , elems :: [CInt]
          } deriving Show

initModel :: Int -> Model 
initModel size = Model {capacity=size, elems=[]}

isEmpty, isFull :: Model  -> Bool
isEmpty Model{..} = null elems
isFull Model{..} = length elems == capacity

-- | execute a command in the functional model
simulate :: Command -> State Model Response
simulate (Enqueue v) = do
  model <- get
  let vs = elems model
  put model {elems = vs++[v]}
  return Nothing

simulate Dequeue = do
  model <- get
  let v:vs = elems model
  put model {elems = vs}
  return (Just v)
  
simulate IsEmpty = do
  model <- get
  return (Just (fromBool $ isEmpty model))

simulate IsFull = do
  model <- get
  return (Just (fromBool $ isFull model))

-- | check if a command is valid 
validCmd :: Command -> State Model Bool
validCmd cmd = do
  model <- get
  let cond = case cmd of
               Enqueue _ -> not (isFull model)
               Dequeue   -> not (isEmpty model)
               _         -> True
  when cond (simulate cmd >> return ())
  return cond

validCmds :: Model -> [Command] -> [Command]
validCmds model cmds = evalState (filterM validCmd cmds) model

shrinkCmds :: Model -> [Command] -> [[Command]]
shrinkCmds model cmds = shrinkMap (validCmds model) id cmds

-- | generate a list of transitions from the initial model:
-- generate an arbitrary list and filter enqueue/dequeue commands
-- that do not satisfy respective pre-conditions
--
transitions :: Model  -> Gen [Command]
transitions model = do
  list <- arbitrary
  return $ validCmds model list

-- | run a command sequence in the model an implementation
-- in lockstep, checking equal replies
stateMachine :: Model -> Queue -> [Command] -> PropertyM IO ()
stateMachine  _     _    [] =
  assert True
stateMachine model queue (cmd:cmds) = do
  let (reply, model') = runState (simulate cmd) model
  reply' <- run (execute queue cmd)
  assert (reply == reply')
  stateMachine model' queue cmds 

-- | correctness property:
-- generate commands and run state machine of
-- model and implementation in lockstep
prop_correct (Positive capacity)
  = forAllShrink (transitions model) (shrinkCmds model) $
       \cmds -> monadicIO $ do
         queue <- run (new $ fromIntegral capacity) 
         stateMachine model queue cmds
         run (delete queue)
  where model = initModel capacity
                                      

main = quickCheck prop_correct 

