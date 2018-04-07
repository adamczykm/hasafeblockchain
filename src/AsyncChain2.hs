{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DeriveFunctor #-}


module AsyncChain2 where


import Data.HList
import Control.Concurrent.Async
-- import Control.Concurrent
-- import Control.Exception.Base
-- import System.Random
import Control.Arrow (second)
import Data.Kind
import Data.Maybe (fromJust)
import Unsafe.Coerce (unsafeCoerce)
-- import Data.Typeable

-------------- utils -------------------

type family ListRev (as :: [x]) :: [x] where
  ListRev xs = ListRevHelper xs '[]

type family ListRevHelper (as :: [*]) (bs :: [*]) :: [*] where
  ListRevHelper '[] bs = bs
  ListRevHelper (x ': xs) bs = ListRevHelper xs (x ': bs)

data AsyncTaskTag = Standalone | StandaloneFallible | Dependent | DependentFallible
  deriving(Eq, Typeable)

newtype NonFallible a = NonFallible {unNonFallible :: a}
  deriving(Eq,Show, Functor)

nonfall :: a -> NonFallible a
nonfall = NonFallible

data AsyncStepTask (t :: AsyncTaskTag) (b0 :: *) (a :: *) (b :: *) :: *  where
  StandaloneTask :: NonFallible (IO (a,  b)) -> AsyncStepTask 'Standalone () a b
  StandaloneTaskFallible :: IO (a, Maybe b) -> AsyncStepTask 'StandaloneFallible () a b
  DependentTask :: NonFallible (b0 -> IO (a, b)) -> AsyncStepTask 'Dependent b0 a b
  DependentTaskFallible :: (b0 -> IO (a, Maybe b)) -> AsyncStepTask 'DependentFallible b0 a b


type family AsyncActionFromTag (t :: AsyncTaskTag) (b0 :: *) (a :: *) (b :: *) :: ret | t -> ret   where
  AsyncActionFromTag 'Standalone c a b =  AsyncStepTask 'Standalone () a b
  AsyncActionFromTag 'StandaloneFallible c a b =  AsyncStepTask 'StandaloneFallible () a b
  AsyncActionFromTag 'DependentFallible b0 a b =  AsyncStepTask 'DependentFallible b0 a b
  AsyncActionFromTag 'Dependent b0 a b =  AsyncStepTask 'Dependent b0 a b

-- type family AsyncActionFromTag (t :: AsyncTaskTag) (b0 :: *) (a :: *) (b :: *) :: ret | t -> ret   where
--   AsyncActionFromTag 'Standalone c a b =  NonFallible (IO (a,  b))
--   AsyncActionFromTag 'StandaloneFallible c a b =  IO (a,  Maybe b)
--   AsyncActionFromTag 'DependentFallible b0 a b =  (b0 -> IO (a, Maybe b))
--   AsyncActionFromTag 'Dependent b0 a b =  NonFallible (b0 -> IO (a, b))


class AsyncAction act tag b0 a b | act -> tag b0 a b where
  toAction :: act -> AsyncStepTask tag b0 a b


instance AsyncAction (NonFallible (IO (a, b))) 'Standalone () a b where
  toAction = StandaloneTask

instance AsyncAction (NonFallible ((->) b0 (IO (a,b)))) 'Dependent b0 a b where
  toAction = DependentTask

instance AsyncAction (IO (a,Maybe b)) 'StandaloneFallible () a b where
  toAction = StandaloneTaskFallible

instance AsyncAction ((->) b0 (IO (a,Maybe b))) 'DependentFallible b0 a b where
  toAction = DependentTaskFallible


test :: AsyncAction act tag b0 a b => act -> AsyncStepTask tag b0 a b
test = toAction


-- type family ToAction (act :: *) :: * where
--   ToAction (IO (a,Maybe b)) = AsyncStepTask 'StandaloneFallible () a b
--   ToAction (IO (a,b)) = AsyncStepTask 'Standalone () a b
--   ToAction ((->) b0 (IO (a,Maybe b))) = AsyncStepTask 'DependentFallible b0 a b
--   ToAction ((->) b0 (IO (a,b))) = AsyncStepTask 'Dependent b0 a b

type family IsNonFallible (act :: *) :: Constraint where
  IsNonFallible (IO (a,b)) = ()
  IsNonFallible ((->) b0 (IO (a,b))) = ()



type family ToNonFallible (act :: *) :: ret | act -> ret where
  ToNonFallible (IO (a,b)) = IO (a, NonFallible b)
  ToNonFallible ((->) b0 (IO (a,b))) = ((->) b0 (IO (a, NonFallible b)))


type family IsStandaloneTag (t :: AsyncTaskTag) :: Constraint where
  IsStandaloneTag 'Standalone = ()
  IsStandaloneTag 'StandaloneFallible = ()


data AsyncChainBuilder :: [AsyncTaskTag] -> [*] -> [*] -> *  where

  AsyncChainStart :: IsStandaloneTag t => AsyncStepTask t () a b
                  -> AsyncChainBuilder (t : '[]) (a : '[]) (b : '[])

  AsyncChainAppend :: AsyncStepTask t b0 a b
                   -> AsyncChainBuilder tgs as (b0 : bs)
                   -> AsyncChainBuilder (t : tgs) (a : as) (b ': b0 ': bs)

start :: (AsyncAction act t () a b, IsStandaloneTag t) => act -> AsyncChainBuilder (t : '[]) (a : '[]) (b : '[])
start = AsyncChainStart . toAction

infixl 1 &:>
(&:>) :: AsyncAction act tag b0 a b =>
         AsyncChainBuilder tgs as (b0 : bs) -> act ->
         AsyncChainBuilder (tag:tgs) (a : as) (b ': b0 ': bs)

bld &:> act = AsyncChainAppend (toAction act) bld


data AsyncStepTag = AsyncStepAutomatic | AsyncStepManual
  deriving(Eq, Typeable)


type family TasksToSteps (tasks :: [AsyncTaskTag]) :: [AsyncStepTag] where
  TasksToSteps '[] = '[]
  TasksToSteps '[t] = TasksToStep '[t]  ': '[]
  TasksToSteps (t ': ts) = TasksToStep (t ': ts) ': TasksToSteps ts


type family TasksToStep (tasks :: [AsyncTaskTag]) :: AsyncStepTag where

  --
  -- WARNING: order of task is in reverse to the tasks in builder, that is
  --          for tasks (t0 : t1 : ts), task t1 depends on task t0

  TasksToStep '[t] = 'AsyncStepAutomatic
  TasksToStep ('StandaloneFallible : 'Dependent : tasks) = 'AsyncStepManual
  TasksToStep ('StandaloneFallible : 'DependentFallible : tasks) = 'AsyncStepManual
  TasksToStep ('DependentFallible : 'Dependent : tasks) = 'AsyncStepManual
  TasksToStep ('DependentFallible : 'DependentFallible : tasks) = 'AsyncStepManual
  TasksToStep ('Dependent : 'Dependent : tasks) = 'AsyncStepAutomatic
  TasksToStep ('Dependent : 'DependentFallible : tasks) = 'AsyncStepAutomatic
  TasksToStep ('Standalone : 'Dependent :tasks) = 'AsyncStepAutomatic
  TasksToStep ('Standalone : 'DependentFallible : tasks) = 'AsyncStepAutomatic
  TasksToStep (t ': 'Standalone ': ts) = 'AsyncStepAutomatic
  TasksToStep (t ': 'StandaloneFallible ': ts) = 'AsyncStepAutomatic


build :: (TasksToStep '[tag0, tag1] ~ z, Typeable z) => AsyncChainBuilder '[ tag1, tag0] '[ u1, u0] '[ r1, r0]
      -> IO (AsyncChain (TasksToSteps '[tag0, tag1]) '[u0, u1] '[r0, r1])
build bld = startStep (bld, undefined) -- startStep (finStep bld)
  where

    finStep :: AsyncChainBuilder (t1 : t0 : ts) (a : xs) (b : y1 : ys)
              -> (AsyncChainBuilder (t0 : ts) xs (y1 : ys),
                  y1 -> IO (AsyncChain '[ TasksToStep '[t0, t1] ] '[a] '[b]))

    finStep bld' = undefined -- step (bld', const $ return AsyncChainDone)


    step :: (TasksToStep '[t0, t1] ~ z, Typeable z) =>
            (AsyncChainBuilder (t1 : t0 : ts) (a : xs) (b : y1 : ys), b -> IO (AsyncChain tgs as bs))
            -> (AsyncChainBuilder (t0 : ts) xs (y1 : ys),
                y1 -> IO (AsyncChain ( TasksToStep '[t0, t1] : tgs) (a : as) (b : bs)))
    step = undefined


    startStep :: (TasksToStep '[t0, t1] ~ z, Typeable z) =>
                 (AsyncChainBuilder '[t1, t0] '[a1, a0] '[b1, b0], b0 -> IO (AsyncChain ts (a1:as) (b1:bs))) ->
                 IO (AsyncChain ( TasksToStep '[t0, t1] : ts) (a0 : a1 : as) (b0 : b1 : bs))
    startStep (currBldr, nextStep) = doAsyncStep () (asyncChainStartAction currBldr) nextStep


    asyncChainStepAction :: AsyncChainBuilder (t : ts) (x : xs) (y : y1 : ys) -> AsyncStepTask t y1 x y
    asyncChainStepAction (AsyncChainAppend action _) = action

    asyncChainStartAction :: AsyncChainBuilder '[t1, t0] '[a1, a0] '[b1, b0] -> (AsyncStepTask t0 () a0 b0, Proxy t1)
    asyncChainStartAction (AsyncChainAppend _ (AsyncChainStart action)) = (action, Proxy)

    doAsyncStep :: forall b0 t0 t1 a b as bs sts z. (TasksToStep '[t0, t1] ~ z, Typeable z) => b0 -> (AsyncStepTask t0 b0 a b, Proxy t1) -> (b -> IO (AsyncChain sts as bs)) -> IO (AsyncChain (TasksToStep '[t0, t1] : sts) (a:as) (b:bs))
    doAsyncStep b0 (task, _) nextStep = do
      asyncedTask <- doAsyncTask b0 task
      return $ temp2 (Proxy :: Proxy t0, Proxy :: Proxy t1) asyncedTask nextStep

    doAsyncTask :: b0 -> AsyncStepTask t0 b0 a b -> IO (Async (a, Maybe b))
    doAsyncTask _  (StandaloneTask (NonFallible act)) = fmap (second Just) <$> async act
    doAsyncTask _  (StandaloneTaskFallible act) = async act
    doAsyncTask b0 (DependentTask (NonFallible act)) = fmap (second Just) <$> async (act b0)
    doAsyncTask b0 (DependentTaskFallible act) = async (act b0)


    temp2 :: forall t0 t1 a b sts as bs z. (TasksToStep '[t0, t1] ~ z, Typeable z) => (Proxy t0, Proxy t1) -> Async (a, Maybe b) ->  (b -> IO (AsyncChain sts as bs)) -> (AsyncChain (TasksToStep '[t0, t1] : sts) (a:as) (b:bs))
    temp2 _ asynced cont = let p = Proxy :: Proxy z in if typeRep p == typeRep (Proxy :: Proxy 'AsyncStepAutomatic)

      then unsafeCoerceTags (AsyncChainStepAuto ((\(a, b) -> (a, cont (fromJust b))) <$> asynced))
      else unsafeCoerceTags (AsyncChainStepManual ((\(a, b) -> (a, b, cont)) <$> asynced))

      where

        -- INFO: unsafeCoerce is used here to assure the compiler of something that is checked in the if condition: that 'AsyncStepManual ~ TasksToStep '[t0, t1]
        unsafeCoerceTags :: forall steptag as' bs'. AsyncChain (steptag : sts) as' bs' -> AsyncChain (TasksToStep '[t0, t1] : sts) as' bs'
        unsafeCoerceTags = unsafeCoerce


    nextSteps :: AsyncChainBuilder (t : ts) (x : xs) (y : y1 : ys) -> AsyncChainBuilder ts xs (y1:ys)
    nextSteps (AsyncChainAppend _ bld) = bld


data AsyncChain :: [AsyncStepTag] -> [*] -> [*] -> * where
  AsyncChainDone :: AsyncChain '[] '[] '[]
  AsyncChainStepAuto :: Async (a, IO (AsyncChain tgs as bs)) -> AsyncChain ('AsyncStepAutomatic : tgs) (a:as) (b:bs)
  AsyncChainStepManual :: Async (a, Maybe b, b -> IO (AsyncChain tgs as bs)) -> AsyncChain ('AsyncStepManual : tgs) (a:as) (b:bs)
  deriving(Typeable)


exampleChainBuilder4 :: AsyncChainBuilder '[ 'Dependent, 'Standalone] '[Integer, Integer] '[Integer, Integer]
exampleChainBuilder4 = start (nonfall (returnIO (1,1))) &:> nonfall (\x -> returnIO (x,x))
  where
    returnIO :: a -> IO a
    returnIO = return
-- (undefined :: AsyncChainBuilder '[ 'Standalone] '[Integer] '[Integer])

-- exampleBuild3 :: AsyncChainBuilder '[ 'Standalone, 'Standalone] '[Integer, Integer] '[Integer, Integer]
--               -> IO (AsyncChain '[ 'AsyncStepAutomatic, 'AsyncStepAutomatic] [Int,Int] [(),()])
-- exampleBuild3 (AsyncChainAppend (StandaloneTask act) bld) = do
--   undefined
--   where


exampleBuild3 :: IO Bool
exampleBuild3  = build asyncChain >>= handleChain
  where
    -- build :: AsyncChainBuilder '[ 'Standalone, 'Standalone] '[Integer, Integer] '[(), ()]
    --               -> IO (AsyncChain '[ 'AsyncStepAutomatic, 'AsyncStepAutomatic] [Integer,Integer] [(),()])
    -- build (AsyncChainAppend (StandaloneTask (NonFallible act)) (AsyncChainStart (StandaloneTask (NonFallible startAct)))) = do

    --   let step = AsyncChainStepAuto . fmap (\(a,_) -> (a, return AsyncChainDone :: IO (AsyncChain '[] '[] '[] ))) <$> async act

    --   AsyncChainStepAuto . fmap (\(a,_) -> (a, step)) <$> async startAct


    asyncChain :: AsyncChainBuilder '[ 'Standalone, 'Standalone] '[Bool, Integer] '[(), ()]
    asyncChain = start (nonfall (returnIO (1,()))) &:> nonfall (returnIO (True,()))


    returnIO :: a -> IO a
    returnIO = return


    handleChain :: AsyncChain '[ 'AsyncStepAutomatic, 'AsyncStepAutomatic] [Integer,Bool] [(),()] -> IO Bool
    handleChain (AsyncChainStepAuto asnc) = do
      (feedback, restChain) <- wait asnc
      print feedback
      restChain >>= handleStep
      where
        handleStep :: AsyncChain '[ 'AsyncStepAutomatic] '[Bool] '[()] -> IO Bool
        handleStep (AsyncChainStepAuto asnca) = do
          (feedback', _) <- wait asnca
          print feedback'
          return feedback'


handletest1 :: AsyncChain '[ 'AsyncStepManual, 'AsyncStepAutomatic] [Int,Int] [Int,()] -> IO Int
handletest1 (AsyncChainStepManual asnc) = do
  (feedback, dep, restChain) <- wait asnc
  print feedback
  case dep of
    Nothing -> return 0
    Just x -> restChain x >>= handleStep
  where
    handleStep :: AsyncChain '[ 'AsyncStepAutomatic] '[Int] '[()] -> IO Int
    handleStep (AsyncChainStepAuto asnca) = do
      (feedback', dep) <- wait asnca
      print feedback'
      return feedback'

    -- testDone (AsyncChainDone) = ()
    -- testDone _ = error "bad"


















--------------- easy building try ------------------------

-- class AsyncAction act where
--   toAction :: act b0 a b -> AsyncStepTask t b0 a b


-- type family ToAction (act :: *) :: * where
--   ToAction (IO (a,Maybe b)) = AsyncStepTask 'StandaloneFallible () a b
--   ToAction (IO (a,b)) = AsyncStepTask 'Standalone () a b
--   ToAction ((->) b0 (IO (a,Maybe b))) = AsyncStepTask 'DependentFallible b0 a b
--   ToAction ((->) b0 (IO (a,b))) = AsyncStepTask 'Dependent b0 a b


-- type family FromAction (tag :: AsyncTaskTag) (act :: *) :: ret | act -> tag ret where
--   FromAction 'StandaloneFallible (AsyncStepTask 'StandaloneFallible c a b) = (IO (a,Maybe b))
--   -- FromAction 'Standalone (AsyncStepTask 'Standalone c a b) = (IO (a,b))
--   FromAction 'DependentFallible (AsyncStepTask 'DependentFallible b0 a b) = ((->) b0 (IO (a,Maybe b)))
--   -- FromAction 'Dependent (AsyncStepTask 'Dependent b0 a b) = ((->) b0 (IO (a,b)))


-- type family ToTag (act :: *) :: ret | act -> ret where
--   ToTag (IO (a,Maybe b)) = 'StandaloneFallible
--   ToTag (IO (a,b)) = 'Standalone
--   ToTag ((->) b0 (IO (a,Maybe b))) = 'DependentFallible
--   ToTag ((->) b0 (IO (a,b))) = 'Dependent



-- test :: (t0 ~ t1) => FromAction t (AsyncStepTask t0 b0 a b) -> AsyncStepTask t1 b0 a b
-- -- test :: FromAction t (AsyncStepTask t b0 a b) -> AsyncStepTask t b0 a b
-- -- test = undefined

-- testUse = (test (return (1,Just 1))) :: AsyncStepTask 'StandaloneFallible () Int Int

-- test a = case a of
--   (act :: IO (a,b)) -> StandaloneTask act
-- -- test (a :: ((->) b0 (IO (a,b)))) = DependentTask a

-- -- test (a :: IO (a,b)) = StandaloneTask a
-- -- test (a :: ((->) b0 (IO (a,b)))) = DependentTask a

-- -- test :: FromAction (AsyncStepTask t b0 a b) -> AsyncChainBuilder tgs as (b0 : bs)
-- --                    -> AsyncChainBuilder (t : tgs) (a : as) (b ': b0 ': bs)
-- -- test = undefined

-- type family F (ins :: [*]) :: out | ins -> out where

-- fun1 :: b0 -> (A t0 b0 a b, Proxy t1) -> (b -> IO (B sts as bs)) -> IO (B (F '[t0, t1] : sts) (a:as) (b:bs))
-- fun1 b0 (task, _) getB = do
--   -- code
--   asynced <- undefined
--   temp2 (Proxy, Proxy) asynced getB

-- temp2 :: (Proxy t0, Proxy t1) -> Async (a, Maybe b) ->  (b -> IO (B sts as bs)) -> IO (B (F '[t0, t1] : sts) (a:as) (b:bs))
-- temp2 = undefined

--  --       error           • Couldn't match type ‘F '[t00, t10]’
--               --                  with ‘F '[t0, t1]’
--               --   Expected type: IO
--               --                    (B (F '[t0, t1] : sts) (a : as) (b : bs))
--               --     Actual type: IO
--               --                    (B (F '[t00, t10] : sts) (a : as) (b : bs))
--               --   NB: ‘F’ is a type function, and may not be injective
--               --   The type variables ‘t00’, ‘t10’ are ambiguous
