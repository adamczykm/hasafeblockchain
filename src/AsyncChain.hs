{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}



module AsyncChain where


import Data.HList
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception.Base
import System.Random


data AsyncChain :: [*] -> * where
  AsyncChainDone :: AsyncChain '[]
  AsyncChainStep :: Async (a, IO (AsyncChain as)) -> AsyncChain (a:as)

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



type family ListRev (as :: [*]) :: [*] where
  ListRev xs = ListRevHelper xs '[]

type family ListRevHelper (as :: [*]) (bs :: [*]) :: [*] where
  ListRevHelper '[] bs = bs
  ListRevHelper (x ': xs) bs = ListRevHelper xs (x ': bs)



data HndlH a b c = HndlS (IO (b,c))
                 | HndlA (a->IO(b,c))


data AsyncChainBuilder :: [*] -> [*] -> *  where
  AsyncChainBuilderStart :: IO (a, b) -> AsyncChainBuilder (a ': '[]) (b ': '[])
  AsyncChainBuilderAppend :: (b0 -> IO (a,b)) -> AsyncChainBuilder as (b0 ': bs) -> AsyncChainBuilder (a ': as) (b ': b0 ': bs)



data AsyncChainHelper :: [*] -> [*] -> * where
  AsyncChainHelperDone :: AsyncChainHelper '[] '[]
  AsyncChainHelperStep :: Async (a, b, b -> IO (AsyncChainHelper as bs)) -> AsyncChainHelper (a:as) (b:bs)



-- buildAsyncChain :: AsyncChainBuilder (a ': as) (b ': bs) -> IO (AsyncChainHelper (ListRev (a ': as)))
-- buildAsyncChain (AsyncChainBuilderAppend action bld) = case bld of
--     (AsyncChainBuilderAppend _ _) -> undefined

-- buildAsyncChain :: AsyncChainBuilder a b -> IO (AsyncChainHelper (ListRev a))
-- buildAsyncChain (AsyncChainBuilderStart action) = async action >>= \s -> return $ AsyncChainHelperStep $ (\(ss,rs) -> (ss,rs,return . const AsyncChainHelperDone)) <$> s
-- buildAsyncChain aschainbld@(AsyncChainBuilderAppend action bld) = undefined--go aschainbld

--   where
--     go (AsyncChainBuilderAppend action bld) = do

--       previousSteps <- go bld

--       undefined
  






  -- collectActions (HCons action HNil) bld >>= undefined
  -- where

-- collectActions :: HList cs -> AsyncChainBuilder (a ': as) (b ': b0 ': bs) -> (HList (HndlH b0 a b  ': cs))
-- collectActions acc (AsyncChainBuilderAppend action bld) = let newacc = HCons (HndlA action) acc in case bld of
--   (AsyncChainBuilderAppend action' bld') -> collectActions newacc bld


collectStartAction :: HList cs -> AsyncChainBuilder (a ': '[] ) (b ': '[] ) -> (HList (HndlH b0 a b  ': cs))
collectStartAction acc (AsyncChainBuilderStart action) = HCons (HndlS action) acc

-- buildAsyncChain (AsyncChainBuilderAppend action bld) = do

--   let lastStep x = AsyncChainHelperStep <$> do
--         asy <- async $ action x
--         return $ (\(a,b) -> (a,b, const $ return AsyncChainHelperDone)) <$> asy

--   (AsyncChainHelperStep prevStepsAsyncChain) <- buildAsyncChain bld


--   return $ AsyncChainHelperStep $ (\(x,y) -> (x,y, lastStep)) <$> prevStepsAsyncChain
--   --   nextAsyncAction = async $ do

--   undefined

-------------- testing
infixr 0 &
(&) :: a -> (a->b) -> b
arg & f = f arg

testAsyncChainBldr :: AsyncChainBuilder '[ Bool, Int ] '[ Bool, Int]
testAsyncChainBldr = AsyncChainBuilderStart (randomIO >>= \x -> return (x,x)) & AsyncChainBuilderAppend (\x -> return (even x, even x))


-------- building async chain -----------------

tempConverter :: AsyncChainBuilder '[ a, b ] '[ c, d ] -> IO (AsyncChainHelper '[ b, a ] '[ d, c ])
tempConverter (AsyncChainBuilderAppend action bld) = do

  let lastStep x = AsyncChainHelperStep <$> do
        asy <- async $ action x
        return $ (\(a,b) -> (a,b, const $ return AsyncChainHelperDone)) <$> asy

  mysteryFun bld lastStep


mysteryFun :: AsyncChainBuilder '[ b ] '[ d ] -> ( d -> IO (AsyncChainHelper '[a] '[c])) -> IO (AsyncChainHelper '[ b, a] '[ d, c])
mysteryFun (AsyncChainBuilderStart action) nextStep = AsyncChainHelperStep <$> do
  asy <- async action
  return $ (\(a,b) -> (a,b, nextStep)) <$> asy



----------------------------------------


testAsyncChainHelper :: IO (AsyncChainHelper '[ Int, Bool] '[Int, Bool])
testAsyncChainHelper = do
  let lastStep x = AsyncChainHelperStep <$> do
        asy <- async $ threadDelay 1500000 >> return (even x, even x)
        return $ (\(a,b) -> (a,b, const $ return AsyncChainHelperDone)) <$> asy

  firstStep <- async $ threadDelay 2000000 >> randomIO >>= \x -> return (x,x)

  return $ AsyncChainHelperStep $ (\(x,y) -> (x,y, lastStep)) <$> firstStep



unHelperTemp :: AsyncChainHelper as bs -> AsyncChain as
unHelperTemp AsyncChainHelperDone = AsyncChainDone
unHelperTemp (AsyncChainHelperStep helperAsync) = AsyncChainStep $ (\(a, b, getNextAsync) -> (a, unHelperTemp <$> getNextAsync b)) <$> helperAsync


testAsyncChain :: IO (AsyncChain '[ Int, Bool])
testAsyncChain = unHelperTemp <$> testAsyncChainHelper


waitHandleSteps :: AsyncChain xs -> HandlerChain xs ys -> IO (HList ys)
waitHandleSteps _ HFin = return HNil
waitHandleSteps (AsyncChainStep asyncedChain) (HandleStep hndle handlers) = do
  (x, startNext) <- wait asyncedChain
  r <- hndle x
  nextStep <- startNext
  HCons r <$> waitHandleSteps nextStep handlers

infixr 1 >:$
(>:$) :: (a -> IO b) -> HandlerChain as bs -> HandlerChain (a : as) (b : bs)
(>:$) = HandleStep


testStepsAsyncChain :: IO ()
testStepsAsyncChain = testAsyncChain >>= \aschain -> void $ waitHandleSteps aschain (print >:$ print >:$ HFin)




-- buildAsyncChain :: AsyncChainBuilder a b -> IO (AsyncChainHelper (ListRev a))
-- buildAsyncChain (AsyncChainBuilderStart action) = async action >>= \s -> return $ AsyncChainHelperStep $ (\(ss,rs) -> (ss,rs,const AsyncChainHelperDone)) <$> s
-- buildAsyncChain (AsyncChainBuilderAppend action bld) = do
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
