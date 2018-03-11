

test1 = do
  safeRet <- fromSafe 0 safeTest
  print safeRet

  where

    safeTest :: Safe IO () Integer
    safeTest = do

      a <- safeLog (Right 10)
      b <- safeLog (Right 0)

      safeLog $ (\x y -> if y == 0 then Left DivisionByZero else Right $ x `div` y) a b -- _ _

-- test1 =













-- -- data Safe s a f = MkSafe s a f
-- -- type ErrorAccum a f = Safe [Error] a f

-- -- getSafeValue :: MonadLog m => Safe (Either [Error] a) m -> m (Maybe a)
-- -- getSafeValue (MkSafe safeState ) = do
  
  
-- --   mapM_ logPrint



  


--  -- save v k

--   -- rewrap :: Safe s a -> () 

-- -- instance Functor (Safe s) where
-- --   fmap f (MkSafe (sf, s)) = MkSafe (f k)


-- newtype Safe f e a = MkSafe {_fromSafe :: [e] -> Either (f [e]) a}


-- instance Functor f => Functor (Safe f e) where
--   fmap f (MkSafe safeVal ) = MkSafe (fmap f . safeVal)


-- instance Monad f => Monad (Safe f e) where
--     return val = MkSafe (const $ Right val)
--     (MkSafe fval) >>= mfun = MkSafe $ \errs -> case fval errs of
--       Left newErrs -> _fromSafe $ mfun newErrs
--         -- Left unhandled -> Left unhandled
--         -- Right f -> Right
        
-- -- instance Applicative f => Applicative (Safe f e) where
-- --     pure val = MkSafe (const $ Right val)
-- --     (MkSafe ffun) <*> (MkSafe fval) = MkSafe $ \errs -> case fval errs of
-- --       Left newErrs -> case (ffun newErrs) of
-- --         Left unhandled -> Left unhandled
-- --         Right f -> Right 

-- -- instance Monad f => Monad (Safe f e s) where
-- --   return = pure
-- --   (MkSafe val) >>= mfun = MkSafe $ \s -> val s >>= \a -> _fromSafe (mfun a) s



-- -- safe :: (s -> Either ff )


--       -- Left ferrs -> Left ferrs
--       -- Right f    -> f <$> fval s

--     -- (MkSafe (Right ffun) _) <*> (MkSafe errs2 safeVal handler) = MkSafe (errs2++errs) (fmap (ffun <*>) safeVal) (\x -> case handler x of
-- --                                                                                                                        Right v -> Right (ffun <*> pure v)
-- --                                                                                                                        Left errs -> Left errs
-- --                                                                                                                        )

-- --   (MkSafe (Left (allErrs, notHandled)) handler) <*>
-- --    (MkSafe (Left (errHandler2, errToHandle2, allErrs2)) handler2) = MkSafe _ (\e -> handler errToHandle2 *> ha
-- --   -- (MkSafe (Left (errHandler, errToHandle, allErrs)) getter) <*>
-- --   --  (MkSafe (Left (errHandler2, errToHandle2, allErrs2)) getter2) = MkSafe (Left (((errHandler *>) getter errToHandle *>) <$> errHandler2, errToHandle2, allErrs2 ++ allErrs)) getter2






-- -- -- apply :: Safe f e a -> (Either [e] a -> Either e b) -> Safe f e b
-- -- -- apply (MkSafe safeVal getter) f = MkSafe (accumErr safeVal f) getter
-- -- --   where
-- -- --     accumErr v@(Left errs) f' = case f' v of
-- -- --       r@(Right _) -> left (const []) r
-- -- --       (Left err) -> Left (err:errs)

-- -- --     accumErr v f' = left (const []) (f' v)


-- -- -- safe :: a -> (forall b. Either [e] b -> f (Either [e] b)) -> Safe f e a
-- -- -- safe safeVal = MkSafe (Right safeVal)


-- -- -- safeGet :: Safe f e a -> f (Either [e] a)
-- -- -- safeGet (MkSafe safeVal getter) = getter safeVal


-- -- -- fromSafe :: Applicative f => a -> Safe f e a -> f a
-- -- -- fromSafe defVal = fromSafeHandle (const defVal)

-- -- -- fromSafeHandle :: Applicative f => ([e] -> a) -> Safe f e a -> f a
-- -- -- fromSafeHandle pur(MkSafe safeVal getter) = _
