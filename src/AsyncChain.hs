{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}



module AsyncChain where


import Data.HList
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception.Base
import System.Random


data AsyncChain :: [*] -> * where
  AsyncChainDone :: AsyncChain '[]
  AsyncChainStep :: Async (a, IO (AsyncChain as)) -> AsyncChain (a:as)

infixr 1 >:$
(>:$) :: (a -> IO b) -> HandlerChain as bs -> HandlerChain (a : as) (b : bs)
(>:$) = HandleStep


data HandlerChain :: [*] -> [*] -> * where
  HFin :: HandlerChain '[] '[]
  HandleStep :: (a -> IO b) -> HandlerChain as bs -> HandlerChain (a ': as) (b ': bs)


data PrefixHList :: [*] -> * where
  PrefixNil :: PrefixHList as
  PrefixCons :: a -> PrefixHList as -> PrefixHList (a:as)



lengthPHL :: PrefixHList xs -> Int
lengthPHL PrefixNil = 0
lengthPHL (PrefixCons _ rst) = 1 + lengthPHL rst


stepAsync :: AsyncChain (a ': as) -> Async (a, IO (AsyncChain as))
stepAsync (AsyncChainStep st) = st


waitAll :: AsyncChain as -> IO (HList as)
waitAll AsyncChainDone = return HNil
waitAll (AsyncChainStep t) = do
  (v, startNext) <- wait t
  rest <- startNext
  HCons v <$> waitAll rest


pollAll :: AsyncChain as -> IO (PrefixHList as, Maybe SomeException)
pollAll AsyncChainDone = return (PrefixNil, Nothing)
pollAll (AsyncChainStep t) = do
  v <- poll t
  case v of
    Nothing -> return (PrefixNil, Nothing)
    Just ei -> case ei of
      Left e -> return (PrefixNil, Just e)
      Right (x,startNext) -> do
        rest <- startNext
        (pfl, err) <- pollAll rest
        return (PrefixCons x pfl, err)

----------



data AsyncChainBuilder :: [*] -> [*] -> *  where
  AsyncChainStart :: IO (a, b) -> AsyncChainBuilder (a : '[]) (b : '[])
  AsyncChainAppend :: (b0 -> IO (a,b)) -> AsyncChainBuilder as (b0 : bs) -> AsyncChainBuilder (a : as) (b ': b0 ': bs)

infixl 1 &:>
(&:>) :: AsyncChainBuilder as (b0 : bs) -> (b0 -> IO (a,b)) -> AsyncChainBuilder (a : as) (b ': b0 ': bs)
(&:>) = flip AsyncChainAppend




data AsyncChainHelper :: [*] -> [*] -> * where
  AsyncChainHelperDone :: AsyncChainHelper '[] '[]
  AsyncChainHelperStep :: Async (a, b, b -> IO (AsyncChainHelper as bs)) -> AsyncChainHelper (a:as) (b:bs)

unHelperTemp :: AsyncChainHelper as bs -> AsyncChain as
unHelperTemp AsyncChainHelperDone = AsyncChainDone
unHelperTemp (AsyncChainHelperStep helperAsync) = AsyncChainStep $ (\(a, b, getNextAsync) -> (a, unHelperTemp <$> getNextAsync b)) <$> helperAsync


-------- building async chain -----------------


start2StepAsyncChain :: AsyncChainBuilder '[ r1, r0 ] '[ a1, a0] -> IO (AsyncChain '[ r0, r1])
start2StepAsyncChain bld = startStep (finStep bld)


start3StepAsyncChain :: AsyncChainBuilder '[ r2, r1, r0 ] '[ a2, a1, a0] -> IO (AsyncChain '[ r0, r1, r2])
start3StepAsyncChain bld = startStep (step (finStep bld))


start4StepAsyncChain :: AsyncChainBuilder '[ r3, r2, r1, r0 ] '[ a3, a2, a1, a0] -> IO (AsyncChain '[ r0, r1, r2, r3])
start4StepAsyncChain bld = startStep (step (step (finStep bld)))

start5StepAsyncChain bld = startStep (step (step (step (finStep bld))))

start6StepAsyncChain bld = startStep (step (step (step (step (finStep bld)))))

start7StepAsyncChain bld = startStep (step (step (step (step (step (finStep bld))))))

start8StepAsyncChain bld = startStep (step (step (step (step (step (step (finStep bld)))))))


step (currBldr, nextStep) = (nextSteps currBldr, \x-> AsyncChainHelperStep .
        fmap (\(a,b) -> (a,b, nextStep)) <$> async (asyncChainStepAction currBldr x))


finStep bld = step (bld, const $ return AsyncChainHelperDone)


startStep (currBldr, nextStep) = unHelperTemp . AsyncChainHelperStep .
  fmap (\(a,b) -> (a,b, nextStep)) <$> async (asyncChainStartAction currBldr)


nextSteps :: AsyncChainBuilder (x : xs) (y : y1 : ys) -> AsyncChainBuilder xs (y1:ys)
nextSteps (AsyncChainAppend _ bld) = bld


asyncChainStepAction  :: AsyncChainBuilder (x : xs) (y : y1 : ys) -> (y1 -> IO (x,y))
asyncChainStepAction  (AsyncChainAppend action _) = action


asyncChainStartAction :: AsyncChainBuilder '[x] '[y] -> IO (x,y)
asyncChainStartAction (AsyncChainStart action) = action

---------- not necessary for now

-- type family ListRev (as :: [*]) :: [*] where
--   ListRev xs = ListRevHelper xs '[]

-- type family ListRevHelper (as :: [*]) (bs :: [*]) :: [*] where
--   ListRevHelper '[] bs = bs
--   ListRevHelper (x ': xs) bs = ListRevHelper xs (x ': bs)



-------------------------------------------------------------------
--------------               testing             ------------------
-------------------------------------------------------------------


infixr 0 &
(&) :: a -> (a->b) -> b
arg & f = f arg

testAsyncChainBldr :: AsyncChainBuilder '[ Bool, Int ] '[ Bool, Int]
testAsyncChainBldr = AsyncChainStart (randomIO >>= \x -> return (x,x)) & AsyncChainAppend (\x -> return (even x, even x))


testAsyncChainHelper :: IO (AsyncChainHelper '[ Int, Bool] '[Int, Bool])
testAsyncChainHelper = do
  let lastStep x = AsyncChainHelperStep <$> do
        asy <- async $ threadDelay 1500000 >> return (even x, even x)
        return $ (\(a,b) -> (a,b, const $ return AsyncChainHelperDone)) <$> asy

  firstStep <- async $ threadDelay 2000000 >> randomIO >>= \x -> return (x,x)

  return $ AsyncChainHelperStep $ (\(x,y) -> (x,y, lastStep)) <$> firstStep



testAsyncChain :: IO (AsyncChain '[ Int, Bool])
testAsyncChain = unHelperTemp <$> testAsyncChainHelper


waitHandleSteps :: AsyncChain xs -> HandlerChain xs ys -> IO (HList ys)
waitHandleSteps _ HFin = return HNil
waitHandleSteps (AsyncChainStep asyncedChain) (HandleStep hndle handlers) = do
  (x, startNext) <- wait asyncedChain
  r <- hndle x
  nextStep <- startNext
  HCons r <$> waitHandleSteps nextStep handlers


testStepsAsyncChain :: IO ()
testStepsAsyncChain = testAsyncChain >>= \aschain -> void $ waitHandleSteps aschain (print >:$ print >:$ HFin)

test1 :: IO Bool
test1 = do
  aschain <- start3StepAsyncChain $ AsyncChainStart ((randomIO :: IO Int) >>= \x->return (x, x)) &:>
                                                     (\x -> heavyComputation >> return (even x, even x)) &:>
                                                     (\x -> heavyComputation >> return (not x, x))

  hLast <$> waitHandleSteps aschain (handleStepResult >:$ handleStepResult >:$ handleStepResult >:$ HFin)


  where handleStepResult x = putStr "handling: " >> print x >> return x
        heavyComputation = threadDelay 1500000





-- buildAsyncChain :: AsyncChainBuilder a b -> IO (AsyncChainHelper (ListRev a))
-- buildAsyncChain (AsyncChainStart action) = async action >>= \s -> return $ AsyncChainHelperStep $ (\(ss,rs) -> (ss,rs,const AsyncChainHelperDone)) <$> s
-- buildAsyncChain (AsyncChainAppend action bld) = do
--   prevStepsAsyncChain <- buildAsyncChain bld
--   nextAsyncAction = async $ do

--   undefined






---- TODO support more async operation

-- Wait for an asynchronous action to complete, and return its value. If the asynchronous action threw an exception, then the exception is re-thrown by wait.

-- wait = atomically . waitSTM

-- poll :: Async a -> IO (Maybe (Either SomeException a))

-- Check whether an Async has completed yet. If it has not completed yet, then the result is Nothing, otherwise the result is Just e where e is Left x if the Async raised an exception x, or Right a if it returned a value a.

-- poll = atomically . pollSTM

-- waitCatch :: Async a -> IO (Either SomeException a)

-- Wait for an asynchronous action to complete, and return either Left e if the action raised an exception e, or Right a if it returned a value a.

-- waitCatch = atomically . waitCatchSTM

-- asyncThreadId :: Async a -> ThreadId

-- Returns the ThreadId of the thread running the given Async.

-- cancel :: Async a -> IO ()

-- Cancel an asynchronous action by throwing the AsyncCancelled exception to it, and waiting for the Async thread to quit. Has no effect if the Async has already completed.

-- cancel a = throwTo (asyncThreadId a) AsyncCancelled <* waitCatch a

-- Note that cancel will not terminate until the thread the Async refers to has terminated. This means that cancel will block for as long said thread blocks when receiving an asynchronous exception.

-- For example, it could block if:

--     It's executing a foreign call, and thus cannot receive the asynchronous exception;
--     It's executing some cleanup handler after having received the exception, and the handler is blocking.

-- uninterruptibleCancel :: Async a -> IO ()

-- Cancel an asynchronous action

-- This is a variant of cancel, but it is not interruptible.

-- cancelWith :: Exception e => Async a -> e -> IO ()

-- Cancel an asynchronous action by throwing the supplied exception to it.

-- cancelWith a x = throwTo (asyncThreadId a) x

-- The notes about the synchronous nature of cancel also apply to cancelWith.

-- data AsyncCancelled

-- The exception thrown by cancel to terminate a thread.
