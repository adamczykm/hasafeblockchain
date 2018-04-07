{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}



module AsyncChain where


import Data.HList
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception.Base
import System.Random
import Data.Kind


data AsyncChain :: [*] -> * where
  AsyncChainDone :: AsyncChain '[]
  AsyncChainStep :: Async (a, IO (AsyncChain as)) -> AsyncChain (a:as)

infixr 1 >:$
(>:$) :: (a -> IO b) -> HandlerChain as bs -> HandlerChain (a : as) (b : bs)
(>:$) = HandleStep

---------- not necessary for now

type family ListRev (as :: [*]) :: [*] where
  ListRev xs = ListRevHelper xs '[]

type family ListRevHelper (as :: [*]) (bs :: [*]) :: [*] where
  ListRevHelper '[] bs = bs
  ListRevHelper (x ': xs) bs = ListRevHelper xs (x ': bs)


data HandlerChain :: [*] -> [*] -> * where
  HFin :: HandlerChain '[] '[]
  HandleStep :: (a -> IO b) -> HandlerChain as bs -> HandlerChain (a ': as) (b ': bs)

data PrefixHandlerChain :: [*] -> [*] -> * where
  PrefHFin :: PrefixHandlerChain as bs
  PrefHandleStep :: (a -> IO b) -> PrefixHandlerChain as bs -> PrefixHandlerChain (a ': as) (b ': bs)



type family IsPrefix (as :: [*]) (bs :: [*]) :: Constraint where
   IsPrefix '[] bs = ()
   IsPrefix (a ': as) (b ': bs) = (a ~ b, IsPrefix as bs)


data PrefixHList :: [*] -> * where
  PrefixNil :: PrefixHList as
  PrefixCons :: a -> PrefixHList as -> PrefixHList (a:as)


data Nat = Z | S Nat

class TEq (a :: Nat) (b :: Nat) where
instance TEq Z Z
instance (TEq a b) => TEq (S a) (S b)

type family Length (as :: [*]) :: Nat where
  Length '[] = Z
  Length (_ ': as) = S (Length as)


type family ListDiff (as :: [*]) (bs :: [*]) :: [*] where
  ListDiff as '[] = as
  ListDiff (a:as) (a:bs) = ListDiff as bs

type family ListPrefixN (n :: Nat) (as :: [*]) :: [*] where
  ListPrefixN Z _ = '[]
  ListPrefixN _ '[] = '[]
  ListPrefixN (S x) (a ': as) = a ': (ListPrefixN x as)



waitHandleSome  :: (IsPrefix xs as, ListDiff xs as ~ d) => AsyncChain as -> HandlerChain xs ys -> IO (AsyncChain (ListDiff as xs), HList ys)
waitHandleSome aschain HFin = return (aschain, HNil)
waitHandleSome (AsyncChainStep asyncedChain) (HandleStep hndle handlers) = do
  (x, startNext) <- wait asyncedChain
  r <- hndle x
  nextStep <- startNext
  (\(a, rs) -> (a, HCons r rs)) <$> waitHandleSome nextStep handlers

waitHandleSteps :: IsPrefix xs xs => AsyncChain xs -> HandlerChain xs ys -> IO (HList ys)
waitHandleSteps hs a = snd <$> waitHandleSome hs a
-- waitHandleSteps _ HFin = return HNil
-- waitHandleSteps (AsyncChainStep asyncedChain) (HandleStep hndle handlers) = do
--   (x, startNext) <- wait asyncedChain
--   r <- hndle x
--   nextStep <- startNext
--   HCons r <$> waitHandleSteps nextStep handlers

-- waitHandleSteps _ HFin = return HNil
-- waitHandleSteps (AsyncChainStep asyncedChain) (HandleStep hndle handlers) = do
--   (x, startNext) <- wait asyncedChain
--   r <- hndle x
--   nextStep <- startNext
--   HCons r <$> waitHandleSteps nextStep handlers

-- prefixHandlerChain :: (Prefixes as bs, Prefixes rs hs) => PrefixHandlerChain as rs -> PrefixHandlerChain bs hs
-- prefixHandlerChain PrefHFin = PrefHFin
-- prefixHandlerChain (PrefHandleStep action chain) = PrefHandleStep action (prefixHandlerChain chain)


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


data AsyncAutoStepTag = AsyncAutomatic | AsyncManual


-- data AsyncChainHelper2 :: AsyncAutoStepTag -> [*] -> [*] -> * where
--   AsyncChainHelperDone2 :: AsyncChainHelper2 AsyncAutomatic '[] '[]
--   AsyncChainHelperStep2 :: Async (a, b, b -> IO (AsyncChainHelper2 as bs)) -> AsyncChainHelper2 (a:as) (b:bs)


type family AutomatedHandlingPossible (a :: AsyncAutoStepTag) (v :: AsyncAutoStepTag) :: AsyncAutoStepTag where
  AutomatedHandlingPossible AsyncAutomatic AsyncAutomatic = AsyncAutomatic
  AutomatedHandlingPossible _              _              = AsyncManual



data AsyncChainBuilder2 :: AsyncAutoStepTag -> [*] -> [*] -> *  where
  AsyncChainStart2 :: IO (a, b) -> AsyncChainBuilder2 AsyncAutomatic (a : '[]) (b : '[])
  AsyncChainAppend2 :: (b0 -> IO (a,b)) -> AsyncChainBuilder2 h as (b0 : bs) -> AsyncChainBuilder2 (AutomatedHandlingPossible AsyncAutomatic h) (a : as) (b ': b0 ': bs)


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



testSome1 :: IO Bool
testSome1 = do
  aschain <- start3StepAsyncChain $ AsyncChainStart ((randomIO :: IO Int) >>= \x->return (x, x)) &:>
                                                     (\x -> heavyComputation >> return (even x, even x)) &:>
                                                     (\x -> heavyComputation >> return (not x, x))

  hLast . snd <$> waitHandleSome aschain (handleStepResult >:$ handleStepResult >:$ HFin)


  where handleStepResult x = putStr "handling: " >> print x >> return x
        heavyComputation = threadDelay 1500000



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
