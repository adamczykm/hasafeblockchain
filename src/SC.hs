module SC where

import Control.Comonad

newtype Safe s a  = MkSafe (s->a,s)

instance Functor (Safe s) where
   fmap f (MkSafe (fs,s)) = MkSafe (f.fs,s)


instance Comonad (Safe s) where
  extract (MkSafe (f,s)) = f s
  duplicate (MkSafe (f,s)) = MkSafe (\x -> MkSafe (f,x), s)


experiment :: Functor f => (s -> f s) -> Safe s a -> f a
experiment unsf (MkSafe (selector, val)) = selector <$> unsf val


-- safe :: (s -> f a) -> s -> Safe s a f
-- safe selectFun initVal = MkSafe (selectFun, initVal)

-- select :: Safe s a f -> f a
-- select (MkSafe (sf,s)) = sf s


-- safeMut :: Safe s a f -> (s -> Safe s2 a f) -> Safe s1 a f
-- safeMut (MkSafe (selectFun, initVal)) safeMutator = MkSafe 

